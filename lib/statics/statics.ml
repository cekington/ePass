open Core 

module I = Intsyn

type ctx = (I.channel * I.typ) list

let _pp_ctx (ctx : ctx) : string = 
  List.fold ~init:"" ~f:(fun acc (c, t) -> acc ^ (I.Print.pp_channel c) ^ " : " ^ (I.Print.pp_typ t) ^ "\n") ctx

let channel_equal (c1 : I.channel) (c2 : I.channel) : bool =
  match (c1, c2) with
  | (I.ChanVar str1, I.ChanVar str2) -> String.equal str1 str2
  | (I.ChanConst i1, I.ChanConst i2) -> i1 = i2
  | _ -> false

let rec lookup_channel (c : I.channel) : ctx -> (I.typ * ctx) option = function 
  | (c', t) :: cs -> 
      if channel_equal c c' then
        Some (t, cs)
      else (
        match (lookup_channel c cs) with
        | None -> None 
        | Some (t', cs') -> Some (t', (c', t) :: cs')
      )
  | [] -> None 

let lookup_channel_one (proc_name : string) (c : I.channel) (context : ctx) : (I.typ * ctx) =
  match lookup_channel c context with
  | None -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is not defined")
  | Some (t, context') -> (t, context')

let lookup_channel_both (proc_name : string) (c : I.channel) (gamma : ctx) (delta : ctx) : (I.typ * I.position * ctx * ctx) =
  match (lookup_channel c gamma, lookup_channel c delta) with
  | (None, None) -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is not defined")
  | (Some (t, gamma'), None) -> (t, I.Antecedent, gamma', delta)
  | (None, Some (t, delta')) -> (t, I.Succedent, gamma, delta')
  | (Some _, Some _) -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is defined in both antecedent and succedent")

let check_no_dup_parms (proc_name : string) : (I.channel * I.typ) list -> unit = function  
  | (c, _) :: ps -> 
      if List.exists ps ~f:(fun (c', _) -> channel_equal c c') then 
        failwith ("proc " ^ proc_name ^ " has duplicate parameters " ^ (I.Print.pp_channel c)) 
  | [] -> ()

let check_all_used (proc_name : string) : ctx -> unit = function
  | (c, _) :: _ -> failwith ("In process " ^  proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " not used") 
  | [] -> ()

let rec find_label_typ_opt (label : string) (env : I.prog) : (string * string) list -> I.typ option = function 
  | (l, t) :: ls -> if String.equal l label then Some (I.expand_env t env) else find_label_typ_opt label env ls
  | [] -> None

let find_label_typ (proc_name : string) (c : I.channel) (label : string) (env : I.prog) (alts : (string * string) list) : I.typ = 
  match find_label_typ_opt label env alts with 
  | Some t -> t
  | None -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ "does not have label " ^ label)

let rec lookup_proc (proc_name : string) (f : string) : I.prog -> (ctx * ctx * bool) = function 
  | (I.TypDef (_, _)) :: ps -> lookup_proc proc_name f ps
  | (I.ProcDef (s, right, left, _)) :: ps -> 
      if String.equal s f then (right, left, false) else lookup_proc proc_name f ps
  | (I.ExnProcDef (s, right, left, _)) :: ps -> 
      if String.equal s f then (right, left, true) else lookup_proc proc_name f ps
  | (I.Exec _) :: ps -> lookup_proc proc_name f ps
  | [] -> failwith ("In process " ^ proc_name ^ ", cannot call process " ^ f ^ " which is undefined")

let rec check_cut_used (proc_name : string) (c : I.channel) : ctx -> unit = function 
  | (c', _) :: cs -> if channel_equal c c' then failwith ("In process " ^ proc_name ^ ", cut channel " ^ (I.Print.pp_channel c) ^ " is not used") else check_cut_used proc_name c cs
  | [] -> ()

(* Gamma |- P :: Delta; Omega *)
let rec typecheck_proc (proc_name : string) (env : I.prog) (gamma : ctx) (delta : ctx) (omega : bool) : I.proc -> (ctx * ctx) = function
  | I.Send (c, msg, p) -> 
    let (tc, placec, gamma', delta') = lookup_channel_both proc_name c gamma delta in (
    match msg with 
    | I.Unit -> (
      match placec with
      | I.Antecedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is in antecedent, cannot send unit to it")
      | I.Succedent -> 
        if I.typ_equal tc I.One then (
          match p with 
          | None -> (gamma', delta')
          | Some _ -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is unit type, cannot have continue process")
        )
        else failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ ": " ^ (I.Print.pp_typ tc) ^ " is not unit type")
    )
    | Label l -> (
        match tc with 
        | I.Plus alts -> (
          match placec with 
          | Antecedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of plus type is in antecedent, cannot send label " ^ l ^ " to it")
          | Succedent -> 
              let evolve_type = find_label_typ proc_name c l env alts in typecheck_optional_proc proc_name env gamma' ((c, evolve_type) :: delta') omega p
        )
        | I.With alts -> (
          match placec with
          | Antecedent -> 
              let evolve_type = find_label_typ proc_name c l env alts in typecheck_optional_proc proc_name env ((c, evolve_type) :: gamma') delta' omega p
          | Succedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of with type is in succedent, cannot send label " ^ l ^ " to it")
        )
        | _ -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ ": " ^ (I.Print.pp_typ tc) ^ " is not a choice type")
      )
    | Channel z ->
      let (tz, placez, gamma'', delta'') = lookup_channel_both proc_name z gamma' delta' in (
        match tc with 
        | I.Tensor (tc1, tc2) -> (
          match placec with 
          | Antecedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of tensor type is in antecedent, cannot send channel " ^ (I.Print.pp_channel z) ^ " to it")
          | Succedent -> (
            match placez with 
            | Antecedent -> 
                if I.typ_equal (I.expand_env tc1 env) tz then typecheck_optional_proc proc_name env gamma'' ((c, (I.expand_env tc2 env)) :: delta'') omega p
                else failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel z) ^ " type mismatch with channel " ^ (I.Print.pp_channel c) ^ " of tensor type")
            | Succedent -> failwith ("In process " ^ proc_name ^ ", cannot send succedent channel " ^ (I.Print.pp_channel z) ^ " through succedent channel " ^ (I.Print.pp_channel c) ^ " of tensor type")
          )
        )
        | I.Par (tc1, tc2) -> (
          match placec with 
          | Antecedent -> (
            match (placez, I.expand_env tc1 env) with 
            | (I.Succedent, I.Dual _) -> 
              failwith ("In process " ^ proc_name ^ ", cannot send succedent channel " ^ (I.Print.pp_channel z) ^ " through antecedent channel " ^ (I.Print.pp_channel c) ^ " of par type")
            | (I.Succedent, _) -> 
              if I.typ_equal (I.expand_env tc1 env) tz then typecheck_optional_proc proc_name env ((c, (I.expand_env tc2 env)) :: gamma'') delta'' omega p
              else failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel z) ^ " type mismatch with channel " ^ (I.Print.pp_channel c)^ " of par type")
            | (I.Antecedent, I.Dual tc1') -> 
              if I.typ_equal (I.expand_env tc1' env) tz then typecheck_optional_proc proc_name env ((c, (I.expand_env tc2 env)) :: gamma'') delta'' omega p
              else failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel z) ^ " type mismatch with channel " ^ (I.Print.pp_channel c)^ " of par type")
            | (I.Antecedent, _) ->
              failwith ("In process " ^ proc_name ^ ", cannot send antecedent channel " ^ (I.Print.pp_channel z) ^ " through antecedent channel " ^ (I.Print.pp_channel c) ^ " of par type")
          )
          | Succedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of par type is in succedent, cannot send channel " ^ (I.Print.pp_channel z) ^ " to it")
        )
        | _ -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ ": " ^ (I.Print.pp_typ tc) ^ " is not a product type")
     )
  )
  | I.Recv (c, k) -> 
    let (tc, placec, gamma', delta') = lookup_channel_both proc_name c gamma delta in (
      match k with 
      | I.ContUnit p -> (
          match placec with
        | I.Antecedent ->
          if I.typ_equal tc I.One then 
            typecheck_proc proc_name env gamma' delta' omega p
          else failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ ": " ^ (I.Print.pp_typ tc) ^ " is not unit type")
        | I.Succedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is in succedent, cannot send unit to it")
      )
      | I.ContLabel b -> 
        let rec remove_alts (b : string) (alts : (string * string) list) : (I.typ * (string * string) list) = (
          match alts with 
          | (l, t) :: ls -> if String.equal l b then ((I.expand_env t env), ls) else 
            let (t', ls') = remove_alts b ls in (t', (l, t) :: ls')
          | [] -> failwith ("In process " ^ proc_name ^ ", choice type of channel " ^ (I.Print.pp_channel c) ^ " does not have label " ^ b)
        ) in
        let rec typecheck_branches (pos : I.position) (branches : (string * I.proc) list) (alts : (string * string) list) (g : ctx) (d : ctx) (o : bool) : (ctx * ctx) = (
          match branches with 
          | (l, p) :: bs -> 
              let (t, alts') = remove_alts l alts in 
              let () = (
                match pos with 
                | I.Antecedent -> check_proc proc_name env ((c, t) :: g) d o p
                | I.Succedent -> check_proc proc_name env gamma ((c, t) :: d) o p
              ) in 
              typecheck_branches pos bs alts' g d o
          | [] -> ( 
            match alts with 
            | [] -> ([], [])
            | (l, _) :: _ -> failwith ("In process " ^ proc_name ^ ", choice type of channel " ^ (I.Print.pp_channel c) ^ " has non-exaustive match, missing label: " ^ l)
          )
        )
        in (
        match tc with 
        | I.Plus alts -> (
          match placec with 
          | I.Antecedent -> typecheck_branches I.Antecedent b alts gamma' delta' omega
          | I.Succedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of plus type is in succedent, cannot recieve branches to it")
        )
        | I.With alts -> (
          match placec with 
          | I.Antecedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of with type is in succedent, cannot recieve branches to it") 
          | I.Succedent -> typecheck_branches I.Succedent b alts gamma' delta' omega
        )
        | _ -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ ": " ^ (I.Print.pp_typ tc) ^ " is not a choice type")
      )
      | I.ContChannel (z, p) -> (
        match tc with 
        | I.Tensor (tc1, tc2) -> (
          match placec with 
          | Antecedent -> typecheck_proc proc_name env ((c, I.expand_env tc2 env) :: ((z, I.expand_env tc1 env) :: gamma')) delta' omega p
          | Succedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of tensor type is in succedent, cannot send channel " ^ (I.Print.pp_channel z) ^ " to it")
        )
        | I.Par (tc1, tc2) -> (
          match placec with 
          | Antecedent -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " of par type is in antecedent, cannot send channel " ^ (I.Print.pp_channel z) ^ " to it")
          | Succedent -> (
            match (I.expand_env tc1 env) with 
            | I.Dual tc1' -> typecheck_proc proc_name env ((z, I.expand_env tc1' env) :: gamma') ((c, I.expand_env tc2 env) :: delta') omega p
            | _ -> typecheck_proc proc_name env gamma' ((c, I.expand_env tc2 env) :: (z, I.expand_env tc1 env) :: delta') omega p
          )
        )
        | _ -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ ": " ^ (I.Print.pp_typ tc) ^ " is not a product type")
     )
  )
  | I.Fwd (c1, c2) ->
    let (tc1, placec1, gamma', delta') = lookup_channel_both proc_name c1 gamma delta in
    let (tc2, placec2, gamma'', delta'') = lookup_channel_both proc_name c2 gamma' delta' in (
      match (placec1, placec2, I.typ_equal tc1 tc2) with
      | (I.Antecedent, I.Succedent, true) -> failwith ("In process " ^ proc_name ^ ", cannot forward succedent channel " ^ (I.Print.pp_channel c1) ^ " to antecedent channel " ^ (I.Print.pp_channel c2))
      | (I.Succedent, I.Antecedent, true) -> (gamma'', delta'') 
      | (_, _, true) -> failwith ("In process " ^ proc_name ^ ", cannot forward channel " ^ (I.Print.pp_channel c1) ^ " and channel " ^ (I.Print.pp_channel c2) ^ ", they are either both antecedent or both succedent")
      | (_, _, false) -> failwith ("In process " ^ proc_name ^ ", cannot forward channel " ^ (I.Print.pp_channel c1) ^ ": " ^ (I.Print.pp_typ tc1) ^ " and channel " ^ (I.Print.pp_channel c2) ^ ": " ^ (I.Print.pp_typ tc2) ^ ", they are not of the same type")
    )
  | I.Call (f, dargs, gargs) -> 
    let (sig_dargs, sig_gargs, is_exn) = lookup_proc proc_name f env in 
    let () = (
      match (is_exn, omega) with 
      | (true, false) -> failwith ("In process " ^ proc_name ^ ", cannot call exceptional process " ^ f ^ " without exceptional channel")
      | _ -> ()
    ) in 
    let rec check_args (context : ctx) (args : I.channel list) (sig_args : ctx) : ctx = (
      match (args, sig_args) with 
      | (c :: cs, (_, t) :: sig_cs) -> 
          let (tc, context') = lookup_channel_one proc_name c context in
          if I.typ_equal tc t then check_args context' cs sig_cs else
            failwith ("In process " ^ proc_name ^ ", cannot call " ^ f ^ ", since arugment channel " ^ (I.Print.pp_channel c) ^ " has a different type")
      | (_ :: _, []) -> failwith ("In process " ^ proc_name ^ ", cannot call " ^ f ^ ", too many arguments")
      | ([], (_, _) :: _) -> failwith ("In process " ^ proc_name ^ ", cannot call " ^ f ^ ", too few arguments")
      | ([], []) -> context
    ) in 
    (check_args gamma gargs sig_gargs, check_args delta dargs sig_dargs)
  | I.Cancel (c, p) -> 
    let (_, _, gamma', delta') = lookup_channel_both proc_name c gamma delta in
    typecheck_optional_proc proc_name env gamma' delta' omega p
  | I.Trycatch (Some (c, t), p1, p2) -> 
    let (gamma', delta') = typecheck_proc proc_name env gamma ((c, t) :: delta) true p1 in
    typecheck_proc proc_name env ((c, t) :: gamma') delta' omega p2
  | I.Trycatch (None, p1, p2) ->
    let (gamma', delta') = typecheck_proc proc_name env gamma delta true p1 in
    typecheck_proc proc_name env gamma' delta' omega p2
  | I.Raise p -> 
    if omega then typecheck_proc proc_name env gamma delta false p else 
    failwith ("In process " ^ proc_name ^ ", raise does not have its corresponding exceptional channel" )
  | I.Cut (c, t, p, q) ->
    let (gamma', delta') = typecheck_proc proc_name env gamma ((c, t) :: delta) false p in
    let () = check_cut_used proc_name c delta' in
    typecheck_proc proc_name env ((c, t) :: gamma') delta' omega q
  | I.Null -> failwith ("In typechecking, there is no null process")

and typecheck_optional_proc (proc_name : string) (env : I.prog) (gamma : ctx) (delta : ctx) (omega : bool) : I.proc option -> (ctx * ctx) = function 
  | Some p -> typecheck_proc proc_name env gamma delta omega p
  | None -> (gamma, delta)

and check_proc (proc_name : string) (env : I.prog) (gamma : ctx) (delta : ctx) (omega : bool) (p : I.proc) : unit =  
  let (gamma', delta') = typecheck_proc proc_name env gamma delta omega p in
  check_all_used proc_name (List.append gamma' delta')

let rec check_exec_proc (str : string) : I.prog -> unit = function 
  | (I.TypDef (_, _)) :: ps -> check_exec_proc str ps
  | (I.ProcDef (s, _, left, _)) :: ps -> 
      if String.equal s str then 
        if List.length left = 0 then
          () 
        else failwith ("Exec process " ^ str ^ " has non empty antecedents")
      else check_exec_proc str ps
  | (I.ExnProcDef (s, _, _, _)) :: ps -> 
      if String.equal s str then 
        failwith ("Exec exceptional process " ^ str)
      else check_exec_proc str ps
  | (I.Exec _) :: ps -> check_exec_proc str ps
  | [] -> failwith ("Exec process " ^ str ^ " which is undefined")

let rec typecheck_main (env : I.prog) : I.prog -> unit = function 
  | (I.TypDef (_, _)) :: ps -> typecheck_main env ps
  | (I.ProcDef (proc_name, delta, gamma, p)) :: ps ->
      let () = check_no_dup_parms proc_name (List.append gamma delta) in
      let () = check_proc proc_name env gamma delta false p in
      typecheck_main env ps
  | (I.ExnProcDef (proc_name, delta, gamma, p)) :: ps ->
      let () = check_no_dup_parms proc_name (List.append gamma delta) in
      let () = check_proc proc_name env gamma delta true p in
      typecheck_main env ps
  | (I.Exec str) :: ps -> 
      let () = check_exec_proc str env in 
      typecheck_main env ps
  | [] -> ()

let typecheck (env : I.prog) : unit = typecheck_main env env