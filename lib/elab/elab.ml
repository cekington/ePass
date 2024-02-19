module E = Extsyn 
module I = Intsyn

let rec is_def_typ_env (typ_var : string) : I.prog -> bool = function
  | d :: ds -> (
    match d with 
    | I.TypDef (str, _) -> (String.equal str typ_var) || (is_def_typ_env typ_var ds)
    | _ -> is_def_typ_env typ_var ds
  )
  | [] -> false

let check_def_typ_env (typ_var : string) (env : I.prog) : unit = 
  if (is_def_typ_env typ_var env) then () else failwith ("Type variable " ^ typ_var ^ " is not defined")

let rec is_def_typ_raw (typ_var : string) : E.prog -> bool = function
  | d :: ds -> (
    match d with 
    | E.TypDef (str, _) -> (String.equal str typ_var) || (is_def_typ_raw typ_var ds)
    | _ -> is_def_typ_raw typ_var ds
  )
  | [] -> false

let check_def_typ_raw (typ_var : string) (raw : E.prog) : unit = 
  if (is_def_typ_raw typ_var raw) then () else failwith ("Type variable " ^ typ_var ^ " is not defined")

let rec find_typ (typ : I.typ) : I.prog -> string option = function
  | d :: ds -> (
    match d with 
    | I.TypDef (str, typ') -> if I.typ_equal typ typ' then Some str else find_typ typ ds
    | _ -> find_typ typ ds
  )
  | [] -> None

let rec check_no_dup_alts (l : string) : (string * E.typ) list -> unit = function 
  | (str, _) :: alts -> 
    if String.equal l str then failwith ("Type has duplicate label " ^ l) else check_no_dup_alts l alts
  | [] -> ()

let rec check_no_dup_branches (l : string) : (string * E.proc) list -> unit = function 
  | (str, _) :: alts -> 
    if String.equal l str then failwith ("Branches have duplicate label " ^ l) else check_no_dup_branches l alts
  | [] -> ()

let rec check_exec_proc_declared (l : string) : E.prog -> unit = function 
  | E.ProcDef (str, _, _, _) :: ds -> 
    if String.equal l str then () else check_exec_proc_declared l ds
  | E.ExnProcDef (str, _, _, _) :: ds -> 
    if String.equal l str then () else check_exec_proc_declared l ds
  | _ :: ds -> check_exec_proc_declared l ds
  | [] -> failwith ("Exec process " ^ l ^ " is not declared")

let rec check_def_twice (str : string) (category : string) : E.prog -> unit = function 
  | E.TypDef (str', _) :: ds -> 
    if String.equal category "type" && String.equal str str' then failwith ("Type " ^ str ^ " is defined twice") else check_def_twice str category ds
  | E.ProcDef (str', _, _, _) :: ds -> 
    if String.equal category "proc" && String.equal str str' then failwith ("Process " ^ str ^ " is defined twice") else check_def_twice str category ds
  | E.ExnProcDef (str', _, _, _) :: ds -> 
    if String.equal category "proc" && String.equal str str' then failwith ("Process " ^ str ^ " is defined twice") else check_def_twice str category ds
  | _ :: ds -> check_def_twice str category ds
  | [] -> ()

let rec elab_typ (raw : E.prog) (env : I.prog) : E.typ -> (I.prog * I.typ) = function
  | E.Var str -> (
    let () = check_def_typ_env str env in 
    (env, I.expand_env str env)
  )
  | E.Tensor (t1, t2) -> 
    let (env1, t1') = name_typ raw env t1 in
    let (env2, t2') = name_typ raw env1 t2 in
    (env2, I.Tensor (t1', t2'))
  | E.Par (t1, t2) -> 
    let (env1, t1') = name_typ raw env t1 in
    let (env2, t2') = name_typ raw env1 t2 in
    (env2, I.Par (t1', t2'))
  | E.Plus alts -> 
    let (env', alts') = elab_alts raw env alts in 
    (env', I.Plus alts')
  | E.With alts -> 
    let (env', alts') = elab_alts raw env alts in 
    (env', I.With alts')
  | E.One -> (env, I.One) 
  | E.Lolli (t1, t2) -> 
    let (env1, t1') = name_typ raw env t1 in
    let (env2, t2') = name_typ raw env1 t2 in
    let dual_name = Util.freshName () in 
    ((I.TypDef (dual_name, Dual t1')) :: env2, I.Par (dual_name, t2'))

and elab_alts (raw : E.prog) (env : I.prog) : ((string * E.typ) list -> (I.prog * (string * string) list)) = function
  | (str, typ) :: alts -> 
    let () = check_no_dup_alts str alts in
    let (env', typ') = name_typ raw env typ in 
    let (env'', alts') = elab_alts raw env' alts in 
    (env'', (str, typ') :: alts')
  | [] -> (env, [])  

and name_typ (raw : E.prog) (env : I.prog) : E.typ -> (I.prog * string) = function
  | E.Var str -> 
      let () = if is_def_typ_env str env then () else check_def_typ_raw str raw in
      (env, str)
  | typ -> 
    let (env', typ') = elab_typ raw env typ in (
      match find_typ typ' env' with 
      | Some str -> (env', str)
      | None -> 
        let str = Util.freshName () in 
        (I.TypDef (str, typ') :: env', str)
    )

let elab_msg : E.msg -> I.msg = function
  | E.Unit -> I.Unit
  | E.Label str -> I.Label str
  | E.Channel str -> I.Channel (I.ChanVar str)

let rec elab_proc (raw : E.prog) (env : I.prog) : E.proc -> (I.prog * I.proc) = function
  | E.Send (str, msg, proc) -> 
    let msg' = elab_msg msg in (
      match proc with 
      | Some p -> 
        let (env', proc') = elab_proc raw env p in
        (env', I.Send (I.ChanVar str, msg', Some proc'))
      | None -> (env, I.Send (I.ChanVar str, msg', None))
    )
  | E.Recv (str, cont) -> 
    let (env', cont') = elab_cont raw env cont in 
    (env', I.Recv (I.ChanVar str, cont'))
  | E.Fwd (str1, str2) -> (env, I.Fwd (I.ChanVar str1, I.ChanVar str2))
  | E.Call (str, chans1, chans2) -> 
    let chans1' = List.map (fun str -> (I.ChanVar str)) chans1 in
    let chans2' = List.map (fun str -> (I.ChanVar str)) chans2 in 
    (env, I.Call (str, chans1', chans2'))
  | E.Cancel (str, proc) -> (
      match proc with 
      | Some p -> 
        let (env', proc') = elab_proc raw env p in
        (env', I.Cancel (I.ChanVar str, Some proc'))
      | None -> (env, I.Cancel (I.ChanVar str, None))
    )
  | E.Trycatch (optstr, proc1, proc2) -> 
    let (env1, proc1') = elab_proc raw env proc1 in
    let (env2, proc2') = elab_proc raw env1 proc2 in (
      match optstr with 
      | None -> 
        (env2, I.Trycatch (None, proc1', proc2'))
      | Some (str, typ) -> 
        let (env3, t') = elab_typ raw env2 typ in
        (env3, I.Trycatch (Some (I.ChanVar str, t'), proc1', proc2'))
    )
  | E.Raise proc -> 
    let (env', proc') = elab_proc raw env proc in
    (env', I.Raise proc')
  | E.Cut (str, typ, proc1, proc2) -> 
    let (env1, proc1') = elab_proc raw env proc1 in
    let (env2, proc2') = elab_proc raw env1 proc2 in 
    let (env3, t') = elab_typ raw env2 typ in
    (env3, I.Cut (I.ChanVar str, t', proc1', proc2'))

and elab_cont (raw : E.prog) (env : I.prog) : E.cont -> (I.prog * I.cont) = function
  | E.ContUnit proc -> 
    let (env', proc') = elab_proc raw env proc in
    (env', I.ContUnit proc')
  | E.ContLabel branches -> 
    let (env', branches') = elab_branches raw env branches in
    (env', I.ContLabel branches')
  | E.ContChannel (str, proc) -> 
    let (env', proc') = elab_proc raw env proc in
    (env', I.ContChannel (I.ChanVar str, proc'))

and elab_branches (raw : E.prog) (env : I.prog) : (string * E.proc) list -> (I.prog * (string * I.proc) list) = function
  | (str, proc) :: bs ->
    let () = check_no_dup_branches str bs in
    let (env', proc') = elab_proc raw env proc in
    let (env'', bs') = elab_branches raw env' bs in
    (env'', (str, proc') :: bs')
  | [] -> (env, []) 

let rec elab_parms (raw : E.prog) (env : I.prog) : (string * E.typ) list -> (I.prog * (I.channel * I.typ) list) = function
  | (str, typ) :: ps -> 
    let (env', typ') = elab_typ raw env typ in 
    let (env'', ps') = elab_parms raw env' ps in 
    (env'', (I.ChanVar str, typ') :: ps')
  | [] -> (env, [])

let rec elab_prog (raw : E.prog) (env : I.prog) : E.prog -> I.prog = function
  | E.TypDef (str, typ) :: ds -> 
    let () = check_def_twice str "type" ds in
    let (env', typ') = elab_typ raw env typ in 
    elab_prog raw (I.TypDef (str, typ') :: env') ds
  | E.ProcDef (str, parms1, parms2, proc) :: ds -> 
    let () = check_def_twice str "proc" ds in
    let (env', parms1') = elab_parms raw env parms1 in 
    let (env'', parms2') = elab_parms raw env' parms2 in 
    let (env''', proc') = elab_proc raw env'' proc in 
    elab_prog raw (I.ProcDef (str, parms1', parms2', proc') :: env''') ds
  | E.ExnProcDef (str, parms1, parms2, proc) :: ds -> 
    let () = check_def_twice str "proc" ds in
    let (env', parms1') = elab_parms raw env parms1 in 
    let (env'', parms2') = elab_parms raw env' parms2 in 
    let (env''', proc') = elab_proc raw env'' proc in 
    elab_prog raw (I.ExnProcDef (str, parms1', parms2', proc') :: env''') ds
  | E.Exec str :: ds -> 
    let () = check_exec_proc_declared str raw in 
    elab_prog raw (I.Exec str :: env) ds
  | [] -> List.rev env

let elab (raw : E.prog) : I.prog = elab_prog raw [] raw