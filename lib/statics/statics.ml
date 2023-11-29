open Core 

module I = Intsyn

type status = 
  (* | Must          must be used *)
  | May           (* may be used or passed on *)
  | Used          (* was used *)

type ctx = (I.channel * I.typ * status) list

let init_ctx = List.map ~f:(fun (c, t) -> (c, t, May))

let check_not_used : status -> bool = function 
  (* | Must -> true *)
  | May -> true
  | Used -> false

let channel_equal (c1 : I.channel) (c2 : I.channel) : bool =
  match (c1, c2) with
  | (I.ChanVar str1, I.ChanVar str2) -> String.equal str1 str2
  | (I.ChanConst i1, I.ChanConst i2) -> i1 = i2
  | _ -> false

let rec lookup_channel (c : I.channel) : ctx -> (ctx option) = function 
  | (c', t, stus) :: cs -> 
      if (channel_equal c c') && (check_not_used stus) then
        Some ((c', t, Used) :: cs)
      else (
        match (lookup_channel c cs) with
        | None -> None 
        | Some cs' -> Some ((c', t, stus) :: cs')
      )
  | [] -> None

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

(* Gamma |- P :: Delta; Omega *)
let rec typecheck_proc (proc_name : string) (env : I.prog) (gamma : ctx) (delta : ctx) (omega : bool) : I.proc -> unit = function
  | I.Cancel (c, p) -> (
    match (lookup_channel c gamma) with
    | Some gamma' -> (
      match p with 
      | Some p -> typecheck_proc proc_name env gamma' delta omega p
      | None -> check_all_used proc_name omega (List.append gamma' delta)
    )
    | None -> (
      match (lookup_channel c delta) with
      | Some delta' -> (
        match p with 
        | Some p -> typecheck_proc proc_name env gamma delta' omega p
        | None -> check_all_used proc_name omega (List.append gamma delta')
      )
      | None -> failwith ("In process " ^ proc_name ^ ", channel " ^ (I.Print.pp_channel c) ^ " not defined")
    )
  )
  | I.Raise p -> 
    if omega then typecheck_proc proc_name env gamma delta false p else 
    failwith ("In process " ^ proc_name ^ ", raise does not have its corresponding exceptional channel" )
  | _ -> failwith "Not implemented"

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
      let () = typecheck_proc str env (init_ctx right) (init_ctx left) false p in
      typecheck env ps
  | (I.ExnProcDef (str, right, left, p)) :: ps ->
      let () = check_no_dup_parms str (List.append left right) in 
      let () = typecheck_proc str env (init_ctx right) (init_ctx left) true p in
      typecheck env ps
  | (I.Exec str) :: ps -> 
      let () = check_exec_proc str env in 
      typecheck env ps
  | [] -> ()
  


