
module I = Intsyn
module Iset = Set.Make(Int)

type procobj = I.proc * I.channel option
type config = procobj list * Iset.t * int

let pp_option (pp : 'a -> string) : 'a option -> string = function 
  | None -> "None"
  | Some x -> "Some " ^ pp x

let pp_config (cfg : config) : string = 
  let (ps, cancelled, num) = cfg in 
  let rec pp_procs (ps : (I.proc * I.channel option) list) : string = 
    match ps with 
    | [] -> ""
    | (p, c) :: ps -> I.Print.pp_proc p ^ ", " ^ pp_option I.Print.pp_channel c ^ "\n" ^ pp_procs ps
  in 
  "num: " ^ string_of_int num ^ "\n" ^ pp_procs ps ^ "\n" ^ "cancelled: " ^ Iset.fold (fun i s -> string_of_int i ^ " " ^ s) cancelled ""

let rec zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list = 
  match (l1, l2) with 
  | ([], []) -> []
  | (x :: xs, y :: ys) -> (x,y) :: zip xs ys 
  | _ -> failwith "zip fail, length of two lists are not equal"

let add_set (s : Iset.t) : I.channel -> Iset.t = function 
  | I.ChanConst i -> Iset.add i s
  | I.ChanVar _ -> failwith "attempt to add ChanVar to canceled set"

let in_set (s : Iset.t) : I.channel -> bool = function 
  | I.ChanConst i -> Iset.mem i s
  | I.ChanVar _ -> failwith "attempt to check ChanVar in canceled set"

let add_chanconst (s : Iset.t) : I.channel -> Iset.t = function 
  | I.ChanConst i -> Iset.add i s
  | I.ChanVar _ -> s

let rec find_proc_all_chanconst (chanconsts : Iset.t) : I.proc -> Iset.t = function 
  | I.Send (c, m, p) -> find_proc_all_chanconst_option (find_msg_all_chanconst (add_chanconst chanconsts c) m) p
  | I.Recv (c, k) -> find_cont_all_chanconst (add_chanconst chanconsts c) k
  | I.Fwd (c, c') -> add_chanconst (add_chanconst chanconsts c) c'
  | I.Call (_, chans1, chans2) -> 
    let chanconsts' = List.fold_left (fun s c -> add_chanconst s c) chanconsts chans1 in 
    List.fold_left (fun s c -> add_chanconst s c) chanconsts' chans2
  | I.Cancel (c, p) -> find_proc_all_chanconst_option (add_chanconst chanconsts c) p
  | I.Trycatch (p1, p2) -> find_proc_all_chanconst (find_proc_all_chanconst chanconsts p1) p2
  | I.Raise p -> find_proc_all_chanconst chanconsts p
  | I.Cut (c, _, child, parent) -> find_proc_all_chanconst (find_proc_all_chanconst (add_chanconst chanconsts c) child) parent
  | I.Null -> chanconsts

and find_proc_all_chanconst_option (chanconsts : Iset.t) : I.proc option -> Iset.t = function 
  | None -> chanconsts
  | Some p -> find_proc_all_chanconst chanconsts p

and find_cont_all_chanconst (chanconsts : Iset.t) : I.cont -> Iset.t = function 
  | I.ContUnit p -> find_proc_all_chanconst chanconsts p
  | I.ContLabel ls -> List.fold_left (fun s (_, p) -> find_proc_all_chanconst s p) chanconsts ls
  | I.ContChannel (_, p) -> find_proc_all_chanconst chanconsts p

and find_msg_all_chanconst (chanconsts : Iset.t) : I.msg -> Iset.t = function 
  | I.Unit -> chanconsts
  | I.Label _ -> chanconsts
  | I.Channel c -> add_chanconst chanconsts c

let cancelAll (p : I.proc) : I.proc option = 
  let all_chanconst : Iset.t = find_proc_all_chanconst Iset.empty p in
  let rec generate_cancel_proc : int list -> I.proc option = function 
  | [] -> None 
  | [n] -> Some (I.Cancel (I.ChanConst n, None))
  | n :: ns -> Some (I.Cancel (I.ChanConst n, generate_cancel_proc ns)) in 
  generate_cancel_proc (Iset.to_list all_chanconst)

let rec select (l : string) : (string * I.proc) list -> I.proc = function 
  | [] -> failwith "select raise Impossible error"
  | (l', p) :: ls -> if String.equal l l' then p else select l ls

let activate (c : I.channel) : I.proc = 
  I.Send (c, I.Label "?act", Some(I.Send (c, I.Unit, None)))

let silent (c : I.channel) : I.proc = 
  I.Send (c, I.Label "?sil", Some(I.Send (c, I.Unit, None)))

let test_need_silent (l : procobj list) : I.channel option -> procobj list = function 
  | None -> l
  | Some c -> (silent c, None) :: l

let reduce (msg : I.msg) (k : I.cont) : I.proc =
  match (msg, k) with 
  | (I.Unit, I.ContUnit p) -> p
  | (I.Label l, I.ContLabel ks) -> select l ks
  | (I.Channel c, I.ContChannel (x, p)) -> I.subst_proc [(c, x)] p
  | _ -> failwith "reduce raise Impossible error" 

let rec split_config (c : I.channel) (c1rev : procobj list) : procobj list -> (procobj list * I.proc * I.channel option * procobj list) option = function 
  | [] -> None 
  | (I.Send (c', msg, p), raise_c) :: c2 -> 
    if I.channel_equal c c' 
      then Some (c1rev, I.Send (c', msg, p), raise_c, c2)
    else split_config c ((I.Send (c', msg, p), raise_c) :: c1rev) c2
  | (I.Recv (c', k), raise_c) :: c2 ->
    if I.channel_equal c c' 
      then Some (c1rev, I.Recv (c', k), raise_c, c2)
    else split_config c ((I.Recv (c', k), raise_c) :: c1rev) c2
  | p :: c2 -> 
    split_config c (p :: c1rev) c2

let rec step_par (env : I.prog) (changed : bool) (cfg : config) (viewed : procobj list) : (bool * config) =
  let (procs, cancelled, num) = cfg in
  match procs with 
  | (p, raise_p) :: ps -> (
    match p with
    | I.Send (c, msg, optp) -> (
      match split_config c ps [] with
      | Some (ps1rev, I.Recv (_, k), raise_r, ps2) -> (
        let reduced_procobj = (reduce msg k, raise_r) in 
        step_par env true ((List.rev ps1rev) @ ps2, cancelled, num) 
        (((match optp with | Some p' -> p' | None -> I.Null), raise_p) :: reduced_procobj :: viewed)
      )
      | None -> step_par env changed (ps, cancelled, num) ((p, raise_p) :: viewed)
      | _ -> failwith "step_par send case raise Impossible error"
    )
    | I.Recv (c, k) -> (
      match split_config c ps [] with
      | Some (ps1rev, I.Send (_, msg, optp), raise_s, ps2) -> (
        let reduced_procobj = (reduce msg k, raise_p) in 
        step_par env true ((List.rev ps1rev) @ ps2, cancelled, num) 
        (((match optp with | Some p' -> p' | None -> I.Null), raise_s) :: reduced_procobj :: viewed)
      )
      | None -> step_par env changed (ps, cancelled, num) ((p, raise_p) :: viewed)
      | _ -> failwith "step_par recv case raise Impossible error"
    )
    | I.Fwd (c, c') -> (
      match split_config c ps [] with
      | Some (ps1rev, anyP, raise_anyP, ps2) -> (
        step_par env true ((List.rev ps1rev) @ ps2, cancelled, num) ((I.Null, raise_p) :: (I.subst_proc [(c, c')] anyP, raise_anyP) :: viewed)
      )
      | None -> step_par env changed (ps, cancelled, num) ((p, raise_p) :: viewed)
    )
    | I.Call (f, chans1, chans2) -> (
      match I.find_proc env f with
      | I.ProcDef (_, delta, gamma, fp) -> (
          let subst1 = zip chans1 (List.map (fun (x, _) -> x) delta) in
          let subst2 = zip chans2 (List.map (fun (x, _) -> x) gamma) in
          let fp' = I.subst_proc (subst1 @ subst2) fp in
          step_par env true (ps, cancelled, num) ((fp', raise_p) :: viewed)
      )
      | I.ExnProcDef (_, delta, gamma, fp) -> (
        let subst1 = zip chans1 (List.map (fun (x, _) -> x) delta) in
        let subst2 = zip chans2 (List.map (fun (x, _) -> x) gamma) in
        let fp' = I.subst_proc (subst1 @ subst2) fp in
        step_par env true (ps, cancelled, num) ((fp', raise_p) :: viewed)
      )
      | _ -> failwith "step_par call case raise Impossible error"
    )
    | I.Cancel (c, optp) -> (
      match optp with 
      | Some p' -> step_par env true (ps, cancelled, num) ((p', raise_p) :: viewed)
      | None -> (true, ((I.Null, raise_p) :: ps, add_set cancelled c, num))
    )
    | I.Trycatch (p1, p2) -> (
      let handleChan = I.ChanConst num in
      let handleActProc = I.Recv (handleChan, I.ContUnit p2) in
      let handleSilProc = match cancelAll p2 with 
      | Some cancelAllProc -> I.Recv (handleChan, I.ContUnit cancelAllProc)
      | None -> I.Recv (handleChan, I.ContUnit I.Null) in
      let handleCont = I.ContLabel [("?sil", handleSilProc); ("?act", handleActProc)] in
      let handleProc = I.Recv (handleChan, handleCont) in
      step_par env true (ps, cancelled, num + 1) ((handleProc, raise_p) :: (p1, Some handleChan) :: viewed)
    )
    | I.Raise p -> (
      match raise_p with 
      | None -> failwith "step_par raise case raise Impossible error"
      | Some c -> step_par env true (ps, cancelled, num) ((activate c, None) :: (p, None) :: viewed)
    )
    | I.Cut (c, _, child, parent) -> (
      let newChan = I.ChanConst num in
      step_par env true (ps, cancelled, num + 1) ((I.subst_proc [newChan, c] child, None) :: (I.subst_proc [(newChan, c)] parent, raise_p) :: viewed)
    )
    | I.Null -> (
      match raise_p with 
      | None -> step_par env true (ps, cancelled, num) viewed
      | Some c -> step_par env true (ps, cancelled, num) ((silent c, None) :: viewed)
    )
  )
  | [] -> (changed, cfg)

let rec iterate (env : I.prog) (cfg : config) : config = (
  match step_par env false cfg [] with 
  | (true, cfg') -> iterate env cfg'
  | (false, cfg') -> cfg' 
)

let rec init (num : int) (depth : int): (I.channel * I.typ) list -> (int * (I.channel * I.channel) list * (I.channel * int) list) = function 
  | [] -> (num, [], [])
  | (c, _) :: cs -> 
    let (num', substs, frontier) = init (num + 1) depth cs in 
    (num', (I.ChanConst num, c) :: substs, (I.ChanConst num, depth) :: frontier)

let rec observe_frontier (env : I.prog) (frontier : (I.channel * int) list) (cfg : config) : string = 
  match frontier with 
  | [] -> ""
  | (c, depth) :: frontiers ->
    I.Print.pp_channel c ^ " -> " ^ (next env (c,depth) frontiers cfg)

and next (env : I.prog) (depchannel : I.channel * int) (frontiers : (I.channel * int) list) (cfg : config) : string = 
  match depchannel with 
  | (_, 0) -> "\n reach maximum depth\n" ^ observe_frontier env frontiers cfg 
  | (c, depth) -> 
    observe env (c, depth) frontiers (iterate env cfg)

and observe (env : I.prog) (depchannel : I.channel * int) (frontiers : (I.channel * int) list) (cfg : config) : string = 
  let (c, depth) = depchannel in 
  let (ps, cancelled, num) = cfg in (
    match split_config c ps [] with 
    | Some (ps1rev, I.Recv (_, k), raise_p, ps2) -> 
      if in_set cancelled c  
      then 
        "Channel " ^ I.Print.pp_channel c ^ " is cancelled.\n" ^
        observe_frontier env frontiers (test_need_silent (List.rev_append ps1rev ps2) raise_p, find_cont_all_chanconst cancelled k, num)
      else 
        "-\n" ^ observe_frontier env frontiers (List.rev_append ps1rev ps2, cancelled, num)
    | Some (ps1rev, I.Send (_, msg, optp), raise_p, ps2) -> 
      if in_set cancelled c 
      then
        let cancelled' = find_msg_all_chanconst cancelled msg in 
        "Channel " ^ I.Print.pp_channel c ^ " is cancelled.\n" ^ (
          match optp with 
          | Some p' -> observe_frontier env frontiers ((p', raise_p) :: (List.rev_append ps1rev ps2), cancelled', num)
          | None -> observe_frontier env frontiers (test_need_silent (List.rev_append ps1rev ps2) raise_p, cancelled', num)
        )
      else 
        failwith "TODO"
    | _ -> failwith "observe raise Impossible error"
  )

let exec (env : I.prog) (proc_name : string) : unit = (
  match (I.find_proc env proc_name) with
  | I.ProcDef (_, delta, _, p) -> 
    let depth = -1 in 
    let (num, substs, frontier) = init 0 depth delta in 
    let p' = I.subst_proc substs p in
    print_string (observe_frontier env frontier ([(p', None)], Iset.empty, num))
  | _ -> failwith "exec raise Impossible error"
) 