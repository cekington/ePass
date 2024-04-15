
module I = Intsyn
module Iset = Set.Make(Int)

type procobj = I.proc * I.channel option
type config = procobj list * Iset.t * int

module IntHashtbl = Hashtbl.Make(Int)

let msgSeq : (string list) IntHashtbl.t = IntHashtbl.create 1
let debug = false

let rec subst_config (subst : (I.channel * I.channel) list) : procobj list -> procobj list = function 
  | [] -> []
  | (p, raise_p) :: ps -> (I.subst_proc subst p, raise_p) :: subst_config subst ps

let safe_find (tbl : 'a IntHashtbl.t) (i : int) : 'a = 
  try IntHashtbl.find tbl i with Not_found -> failwith "safe_find raise Impossible error"

let translate_msg : unit -> string = fun () ->
  IntHashtbl.fold (fun key value acc -> (I.Print.pp_channel (I.ChanConst key)) ^ " -> " ^ (String.concat "." (List.rev value)) ^ "\n" ^ acc) msgSeq ""

let add_msg (msg : string) : I.channel -> unit = function 
  | I.ChanConst i ->
    let msgs = safe_find msgSeq i in 
    IntHashtbl.replace msgSeq i (msg :: msgs)
  | I.ChanVar _ -> failwith "add_msg raise Impossible error"

let print_debug (f : 'a -> unit) (x : 'a) : unit =
  if debug then f x else ()

let pp_option (pp : 'a -> string) : 'a option -> string = function 
  | None -> "_"
  | Some x -> pp x

let get_chanconst : I.channel -> int = function 
  | I.ChanConst i -> i
  | I.ChanVar _ -> failwith "get_chanconst raise Impossible error"

let pp_config (cfg : config) : string = 
  let (ps, cancelled, num) = cfg in 
  let rec pp_procs (ps : (I.proc * I.channel option) list) : string = 
    match ps with 
    | [] -> ""
    | (p, c) :: ps -> "(" ^ I.Print.pp_proc p ^ ") with exception channal: " ^ pp_option I.Print.pp_channel c ^ "\n" ^ pp_procs ps
  in 
  "num: " ^ string_of_int num ^ "\n" ^ "procs:\n" ^ pp_procs ps ^ "cancelled: {" ^ String.concat "," (List.map (fun i -> I.Print.pp_channel (I.ChanConst i)) (Iset.elements cancelled)) ^ "}\n"

let pp_frontier (frontier : (I.channel * int) list) : string = 
  let rec pp_frontiers (frontier : (I.channel * int) list) : string = 
    match frontier with 
    | [] -> ""
    | (c, depth) :: frontiers -> "(" ^ I.Print.pp_channel c ^ ", " ^ string_of_int depth ^ ")\n" ^ pp_frontiers frontiers
  in 
  "frontier:\n" ^ pp_frontiers frontier

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

let subst_set (subst : I.channel * I.channel) (s : Iset.t) : Iset.t = 
  match subst with
  | (I.ChanConst c, I.ChanConst c') -> 
    if Iset.mem c' s then Iset.add c (Iset.remove c' s) else s
  | _ -> failwith "attempt to subst ChanVar in canceled set"

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
  | I.Trycatch (Some (c, _), p1, p2) -> find_proc_all_chanconst (find_proc_all_chanconst (add_chanconst chanconsts c) p1) p2
  | I.Trycatch (None, p1, p2) -> find_proc_all_chanconst (find_proc_all_chanconst chanconsts p1) p2
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

let rec generate_cancel_proc : int list -> I.proc option = function 
  | [] -> None 
  | [n] -> Some (I.Cancel (I.ChanConst n, None))
  | n :: ns -> Some (I.Cancel (I.ChanConst n, generate_cancel_proc ns))

let cancelAllcont (k : I.cont) : I.proc option = 
  let all_chanconst : Iset.t = find_cont_all_chanconst Iset.empty k in 
  generate_cancel_proc (Iset.to_list all_chanconst)

let cancelAll (p : I.proc) : I.proc option = 
  let all_chanconst : Iset.t = find_proc_all_chanconst Iset.empty p in
  generate_cancel_proc (Iset.to_list all_chanconst)

let rec select (l : string) : (string * I.proc) list -> I.proc option = function 
  | [] -> None
  | (l', p) :: ls -> if String.equal l l' then Some p else select l ls

let activate (c : I.channel) : I.proc = 
  I.Send (c, I.Label "?act", Some(I.Send (c, I.Unit, None)))

let silent (c : I.channel) : I.proc = 
  I.Send (c, I.Label "?sil", Some(I.Send (c, I.Unit, None)))

let test_need_silent (l : procobj list) : I.channel option -> procobj list = function 
  | None -> l
  | Some c -> (silent c, None) :: l

let reduce (msg : I.msg) (k : I.cont) : I.proc option =
  match (msg, k) with 
  | (I.Unit, I.ContUnit p) -> Some p
  | (I.Label l, I.ContLabel ks) -> select l ks
  | (I.Channel c, I.ContChannel (x, p)) -> Some (I.subst_proc [(c, x)] p)
  | _ -> failwith "reduce raise Impossible error" 

let rec split_config (c : I.channel) (c1rev : procobj list) : procobj list -> (procobj list * I.proc * I.channel option * procobj list) option = function 
  | [] -> None 
  | (I.Send (c', msg, optp), raise_c) :: c2 -> 
    if I.channel_equal c c' 
      then Some (c1rev, I.Send (c', msg, optp), raise_c, c2)
    else split_config c ((I.Send (c', msg, optp), raise_c) :: c1rev) c2
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
      match split_config c [] ps with
      | Some (ps1rev, I.Recv (_, k), raise_r, ps2) -> (
        let reduced_procobjs : procobj = (
          match reduce msg k with
        | None -> (
          match cancelAllcont k with
          | None -> (I.Raise I.Null, raise_r)
          | Some cancelProc -> (I.Raise cancelProc, raise_r)
        )
        | Some reduce_p -> (reduce_p, raise_r)
        ) in 
        step_par env true ((List.rev ps1rev) @ ps2, cancelled, num) 
        (((match optp with | Some p' -> p' | None -> I.Null), raise_p) :: reduced_procobjs :: viewed)
      )
      | None -> step_par env changed (ps, cancelled, num) ((p, raise_p) :: viewed)
      | _ -> failwith "step_par send case raise Impossible error"
    )
    | I.Recv (c, k) -> (
      match split_config c [] ps with
      | Some (ps1rev, I.Send (_, msg, optp), raise_s, ps2) -> (
        let reduced_procobj = (
          match reduce msg k with 
          | None -> (
            match cancelAllcont k with 
            | None -> (I.Raise I.Null, raise_p)
            | Some cancelProc -> (I.Raise cancelProc, raise_p)
          )
          | Some reduce_p -> (reduce_p, raise_p)
        ) in 
        step_par env true ((List.rev ps1rev) @ ps2, cancelled, num) 
        (((match optp with | Some p' -> p' | None -> I.Null), raise_s) :: reduced_procobj :: viewed)
      )
      | None -> step_par env changed (ps, cancelled, num) ((p, raise_p) :: viewed)
      | _ -> failwith "step_par recv case raise Impossible error"
    )
    | I.Fwd (c, c') -> (
      let cfg' = (subst_config [(c, c')] ps, subst_set (c, c') cancelled, num) in
      step_par env true cfg' ((I.Null, raise_p) :: (subst_config [(c, c')] viewed))
    )
    | I.Call (f, chans1, chans2) -> (
      match I.find_proc env f with
      | I.ProcDef (_, delta, gamma, fp) -> (
        let subst1 = zip chans1 (List.map (fun (x, _) -> x) delta) in
        let subst2 = zip chans2 (List.map (fun (x, _) -> x) gamma) in
        let fp' = I.subst_proc (subst1 @ subst2) fp in
        step_par env true (ps, cancelled, num) ((fp', raise_p) :: viewed)
      )
      | _ -> failwith "step_par call case raise Impossible error"
    )
    | I.Cancel (c, optp) -> 
      step_par env true (ps, add_set cancelled c, num) (((match optp with | Some p' -> p' | None -> I.Null), raise_p) :: viewed)
    | I.Trycatch (optc, p1, p2) -> (
      let handleChan = I.ChanConst num in
      let handleActProc = I.Recv (handleChan, I.ContUnit p2) in
      let handleSilProc = match cancelAll p2 with 
      | Some cancelAllProc -> I.Recv (handleChan, I.ContUnit cancelAllProc)
      | None -> I.Recv (handleChan, I.ContUnit I.Null) in
      let handleCont = I.ContLabel [("?sil", handleSilProc); ("?act", handleActProc)] in
      let handleProc = I.Recv (handleChan, handleCont) in (
        match optc with 
        | Some (c, _) -> (
          let newChan = I.ChanConst (num + 1) in
          step_par env true (ps, cancelled, num + 2) ((I.subst_proc [(newChan, c)] handleProc, raise_p) :: (I.subst_proc [(newChan, c)] p1, Some handleChan) :: viewed)
        )
        | None -> 
          step_par env true (ps, cancelled, num + 1) ((handleProc, raise_p) :: (p1, Some handleChan) :: viewed)
      )
    )
    | I.Raise p -> (
      match raise_p with 
      | None -> failwith "Uncaught exception, process aborted"
      | Some c -> step_par env true (ps, cancelled, num) ((activate c, None) :: (p, None) :: viewed)
    )
    | I.Cut (c, _, child, parent) -> (
      let newChan = I.ChanConst num in
      step_par env true (ps, cancelled, num + 1) ((I.subst_proc [newChan, c] child, None) :: (I.subst_proc [(newChan, c)] parent, raise_p) :: viewed)
    )
    | I.Null -> 
      step_par env true (ps, cancelled, num) (test_need_silent viewed raise_p)
  )
  | [] -> (changed, (List.rev viewed, cancelled, num))

let rec iterate (env : I.prog) (cfg : config) : config = 
  let (changed, cfg') = step_par env false cfg [] in
  let () = print_debug print_string ("-----Config " ^ (if changed then "(changed)" else "(unchanged)") ^ "-----\n" ^ pp_config cfg' ^ "\n") in 
  if changed then iterate env cfg' else cfg'

let rec init (num : int) (depth : int): (I.channel * I.typ) list -> (int * (I.channel * I.channel) list * (I.channel * int) list) = function 
  | [] -> (num, [], [])
  | (c, _) :: cs -> 
    let (num', substs, frontier) = init (num + 1) depth cs in 
    let () = IntHashtbl.add msgSeq num [] in
    (num', (I.ChanConst num, c) :: substs, (I.ChanConst num, depth) :: frontier)

let observe (depchannel : I.channel * int) (frontiers : (I.channel * int) list) 
    (visited : (I.channel * int) list) (observed : bool) (cfg : config) : (bool * (I.channel * int) list * (I.channel * int) list * config) = 
  let (c, depth) = depchannel in 
  let (ps, cancelled, num) = cfg in (
    match split_config c [] ps with 
    | Some (ps1rev, I.Recv (_, k), raise_p, ps2) -> 
      if in_set cancelled c  
      then 
        let () = add_msg "cancelled" c in
        (true, frontiers, visited, (test_need_silent (List.rev_append ps1rev ps2) raise_p, find_cont_all_chanconst cancelled k, num))
      else 
        let () = add_msg "-\n" c in
        (true, frontiers, visited, (test_need_silent (List.rev_append ps1rev ps2) raise_p, cancelled , num))
    | Some (ps1rev, I.Send (_, msg, optp), raise_p, ps2) ->
      let new_procobj : procobj = ((match optp with | Some p' -> p' | None -> I.Null), raise_p) in
      if in_set cancelled c 
      then
        let cancelled' = find_msg_all_chanconst cancelled msg in 
        let () = add_msg "cancelled" c in
        (true, frontiers, visited, (new_procobj :: (List.rev_append ps1rev ps2), cancelled', num))
      else (
        match msg with 
        | I.Unit ->
          let () = add_msg "()" c in
          (true, frontiers, visited, (new_procobj :: (List.rev_append ps1rev ps2), cancelled, num))
        | I.Label l -> 
          let () = add_msg l c in
          (true, ((c, depth - 1) :: frontiers), visited, (new_procobj :: (List.rev_append ps1rev ps2), cancelled, num))
        | I.Channel c' -> 
          let () = add_msg (I.Print.pp_channel c') c in
          let () = if IntHashtbl.mem msgSeq (get_chanconst c') then () else IntHashtbl.add msgSeq (get_chanconst c') [] in
          (true, ((c, depth - 1) :: (c', depth - 1) :: frontiers), visited, (new_procobj :: (List.rev_append ps1rev ps2), cancelled, num))
      )
    | None -> 
      if in_set cancelled c 
      then
        let () = add_msg "cancelled" c in
        (true, frontiers, visited, (ps, cancelled, num))
      else 
        (observed, frontiers, ((c, depth) :: visited), (ps, cancelled, num))
    | _ -> failwith "observe raise Impossible error"
  )

let rec observe_frontier (env : I.prog) (frontier : (I.channel * int) list) 
  (visited : (I.channel * int) list) (observed : bool) (cfg : config) : ((I.channel * int) list * config) option = 
  match (frontier, visited) with 
  | ([], []) -> None
  | ([], _) -> if observed then Some (visited, cfg) else failwith "observe_frontier raise Impossible error"
  | ((c, 0) :: frontiers, _) ->
    let () = add_msg "reach maximum depth" c in 
    observe_frontier env frontiers visited true cfg
  | ((c, depth) :: frontiers, _) -> 
    let (observed', frontier', visited', cfg') = observe (c, depth) frontiers visited observed cfg in
    observe_frontier env frontier' visited' observed' cfg'

let rec new_iteration (env : I.prog) (frontier : (I.channel * int) list) (cfg : config) : unit =
  let () = print_debug print_string "-----Config-----\n" in
  let () = print_debug print_string (pp_config cfg) in
  let cfg' = iterate env cfg in 
  let observe_result = observe_frontier env frontier [] false cfg' in
  let () = print_debug print_string "-----Observing Message Sequence-----\n" in
  let () = print_debug print_string (translate_msg ()) in
  let () = print_debug print_string "------------------------------------\n" in
  match observe_result with 
  | None -> ()
  | Some (frontier', cfg'') -> new_iteration env frontier' cfg''

let exec_one (env : I.prog) (proc_name : string) : unit = (
  match (I.find_proc env proc_name) with
  | I.ProcDef (_, delta, _, p) -> 
    let depth = -1 in 
    let (num, substs, frontier) = init 0 depth delta in 
    let p' = I.subst_proc substs p in
    let () = print_debug print_string (pp_frontier frontier) in
    new_iteration env frontier ([(p', None)], Iset.empty, num)
  | _ -> failwith "exec raise Impossible error"
) 

let rec exec_helper (env : I.prog) : I.prog -> string = function 
  | [] -> ""
  | [I.Exec f] -> 
    let () = IntHashtbl.clear msgSeq in (
    try 
      let () = exec_one env f in
      let thisMSG = translate_msg () in
      "Executing process " ^ f ^ ":\n" ^ thisMSG
    with Failure e ->
      "Executing process " ^ f ^ ":\n" ^ e
    )
  | (I.Exec f) :: envs -> 
    let nextMSGs = exec_helper env envs in (
    try 
      let () = IntHashtbl.clear msgSeq in
      let () = exec_one env f in
      let thisMSG = translate_msg () in
      "Executing process " ^ f ^ ":\n" ^ thisMSG ^ "\n" ^ nextMSGs
    with Failure e ->
      "Executing process " ^ f ^ ":\n" ^ e ^ "\n" ^ nextMSGs
    )
  | _ :: envs -> exec_helper env envs 

let exec (env : I.prog) : string = 
  exec_helper env env