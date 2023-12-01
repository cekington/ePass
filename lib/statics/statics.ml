open Core 

module I = Intsyn

type ctx = (I.channel * I.typ * I.status) list

let init_ctx = List.map ~f:(fun (c, t) -> (c, t, I.Must))

let check_not_used : I.status -> bool = function 
  | I.Must -> true
  | I.May -> true
  | I.Used -> false

let _must_to_may (context : ctx) : ctx = List.map context ~f:(function (c, t, I.Must) -> (c, t, I.May) | (c, t, stus) -> (c, t, stus))

let channel_equal (c1 : I.channel) (c2 : I.channel) : bool =
  match (c1, c2) with
  | (I.ChanVar str1, I.ChanVar str2) -> String.equal str1 str2
  | (I.ChanConst i1, I.ChanConst i2) -> i1 = i2
  | _ -> false

let rec lookup_channel (c : I.channel) : ctx -> (I.typ * ctx) option = function 
  | (c', t, stus) :: cs -> 
      if (channel_equal c c') && (check_not_used stus) then
        Some (t, (c', t, Used) :: cs)
      else (
        match (lookup_channel c cs) with
        | None -> None 
        | Some (t, cs') -> Some (t, (c', t, stus) :: cs')
      )
  | [] -> None 

let lookup_channel_both (proc_name : string) (c : I.channel) (gamma : ctx) (delta : ctx) : (I.typ * I.position * ctx * ctx) =
  match (lookup_channel c gamma, lookup_channel c delta) with
  | (None, None) -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is not defined")
  | (Some (t, gamma'), None) -> (t, Antecedent, gamma', delta)
  | (None, Some (t, delta')) -> (t, Succedent, gamma, delta')
  | (Some _, Some _) -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is defined in both antecedent and succedent")

let rec flip_dual (env : I.prog) (gamma : ctx) (delta : ctx) : (ctx * ctx) =
  match (gamma, delta) with 
  | ((c, I.Dual str, stus) :: gs, _) -> 
      let (gamma', delta') = flip_dual env gs delta in 
      (gamma', (c, I.expand_env str env, stus) :: delta')
  | ((c, t, stus) :: gs, _) -> 
      let (gamma', delta') = flip_dual env gs delta in 
      ((c, t, stus) :: gamma', delta')
  | ([], (c, I.Dual str, stus) :: ds) -> 
      let (gamma', delta') = flip_dual env gamma ds in 
      ((c, I.expand_env str env, stus) :: gamma', delta')
  | ([], (c, t, stus) :: ds) -> 
      let (gamma', delta') = flip_dual env gamma ds in 
      (gamma', (c, t, stus) :: delta')
  | ([], []) -> ([], [])

let check_no_dup_parms (proc_name : string) : (I.channel * I.typ) list -> unit = function  
  | (c, _) :: ps -> 
      if List.exists ps ~f:(fun (c', _) -> channel_equal c c') then 
        failwith ("proc " ^ proc_name ^ " has duplicate parameters " ^ (I.Print.pp_channel c)) 
  | [] -> ()

let check_all_used (proc_name : string) (omega : bool) : ctx -> unit = 
  if omega then failwith ("In process " ^  proc_name ^ ", exceptional channel not used") else
  List.iter ~f:(fun (c, _, stus) -> 
    if check_not_used stus then failwith ("In process " ^  proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " not used") else ()
  )

let rec find_label_typ_opt (label : string) (env : I.prog) : (string * string) list -> I.typ option = function 
  | (l, t) :: ls -> if String.equal l label then Some (I.expand_env t env) else find_label_typ_opt label env ls
  | [] -> None

let find_label_typ (proc_name : string) (c : I.channel) (label : string) (env : I.prog) (alts : (string * string) list) : I.typ = 
  match find_label_typ_opt label env alts with 
  | Some t -> t
  | None -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ "does not have label " ^ label)

(* Gamma |- P :: Delta; Omega *)
let rec typecheck_proc (proc_name : string) (env : I.prog) (g : ctx) (d : ctx) (omega : bool) (process : I.proc) : (ctx * ctx * bool) = 
  let (gamma, delta) = flip_dual env g d in
  match process with
  | I.Send (c, msg, p) -> (
    match msg with 
    | I.Unit -> (
      let (t, place, gamma', delta') = lookup_channel_both proc_name c gamma delta in (
      match place with
      | Antecedent ->
        failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is in antecedent, cannot send unit to it")
      | Succedent -> 
          if I.typ_equal t I.One then (
            match p with 
            | None -> (gamma', delta', omega)
            | Some _ -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is unit type, cannot have continue process")
          )
          else failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " is not unit type")
      )
    )
    | Label l -> (
      let (t, place, gamma', delta') = lookup_channel_both proc_name c gamma delta in (
        match (place, t) with 
        | (I.Antecedent, I.With alts) -> 
            let evolve_type = find_label_typ proc_name c l env alts in 
            typecheck_optional_proc proc_name env ((c, evolve_type, I.Must) :: gamma') delta' omega p
        | (I.Succedent, I.Plus alts) -> 
            let evolve_type = find_label_typ proc_name c l env alts in 
            typecheck_optional_proc proc_name env gamma' ((c, evolve_type, I.Must) :: delta') omega p
        | _ -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " has incorrect choice type")
      )
    )
    | _ -> failwith "Not implemented"
  )
  | I.Cancel (c, p) -> 
    let (_, _, gamma', delta') = lookup_channel_both proc_name c gamma delta in
    typecheck_optional_proc proc_name env gamma' delta' omega p
  | I.Raise p -> 
    if omega then typecheck_proc proc_name env gamma delta false p else 
    failwith ("In process " ^ proc_name ^ ", raise does not have its corresponding exceptional channel" )
  | _ -> failwith "Not implemented"

and typecheck_optional_proc (proc_name : string) (env : I.prog) (gamma : ctx) (delta : ctx) (omega : bool) : I.proc option -> (ctx * ctx * bool) = function 
  | Some p -> typecheck_proc proc_name env gamma delta omega p
  | None -> (gamma, delta, omega)

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
  | (I.Exec str) :: ps -> check_exec_proc str ps
  | [] -> failwith "check_exec_proc raise Impossible error"

let rec typecheck (env : I.prog) : I.prog -> unit = function 
  | (I.TypDef (_, _)) :: ps -> typecheck env ps
  | (I.ProcDef (str, right, left, p)) :: ps ->
      let () = check_no_dup_parms str (List.append left right) in 
      let (gamma, delta, omega) = typecheck_proc str env (init_ctx left) (init_ctx right) false p in
      let () = check_all_used str omega (List.append gamma delta) in
      typecheck env ps
  | (I.ExnProcDef (str, right, left, p)) :: ps ->
      let () = check_no_dup_parms str (List.append left right) in 
      let (gamma, delta, omega) = typecheck_proc str env (init_ctx left) (init_ctx right) true p in
      let () = check_all_used str omega (List.append gamma delta) in
      typecheck env ps
  | (I.Exec str) :: ps -> 
      let () = check_exec_proc str env in 
      typecheck env ps
  | [] -> ()
  


