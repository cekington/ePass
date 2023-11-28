open Core

module E = Extsyn

type channel = 
  | ChanVar of string
  | ChanConst of int 

type typ = 
  | Tensor of string * string 
  | Par of string * string
  | Plus of (string * string) list
  | With of (string * string) list
  | One 
  | Lolli of string * string

type msg = 
  | Unit
  | Label of string
  | Channel of channel

type proc = 
  | Send of channel * msg * proc option
  | Recv of channel * cont 
  | Fwd of channel * channel
  | Call of string * channel list * channel list * proc option
  | Cancel of channel * proc option
  | Trycatch of proc * proc
  | Raise of proc 
  | Cut of channel * typ option * proc * proc

and cont = 
  | ContUnit of proc 
  | ContLabel of (string * proc) list
  | ContChannel of channel * proc

type def = 
  | TypDef of string * typ
  | ProcDef of string * (channel * typ) list * (channel * typ) list * proc
  | ExnProcDef of string * (channel * typ) list * (channel * typ) list * proc
  | Exec of string 

type prog = def list

let counter = ref 0

let freshName () =
  let name = "%tp_" ^ (string_of_int !counter) in
  counter := !counter + 1;
  name

let rec expand_env (typ_var : string) : prog -> typ = function
  | d :: ds -> (
    match d with 
    | TypDef (str, typ) -> if String.equal str typ_var then typ else expand_env typ_var ds
    | _ -> expand_env typ_var ds
  )
  | [] -> failwith "Impossible"

let rec is_def_typ_env (typ_var : string) : prog -> bool = function
  | d :: ds -> (
    match d with 
    | TypDef (str, _) -> (String.equal str typ_var) || (is_def_typ_env typ_var ds)
    | _ -> is_def_typ_env typ_var ds
  )
  | [] -> false

let check_def_typ_env (typ_var : string) (env : prog) : unit = 
  if (is_def_typ_env typ_var env) then () else failwith ("Type variable " ^ typ_var ^ " is not defined")

let rec is_def_typ_raw (typ_var : string) : E.prog -> bool = function
  | d :: ds -> (
    match d with 
    | TypDef (str, _) -> (String.equal str typ_var) || (is_def_typ_raw typ_var ds)
    | _ -> is_def_typ_raw typ_var ds
  )
  | [] -> false

let check_def_typ_raw (typ_var : string) (raw : E.prog) : unit = 
  if (is_def_typ_raw typ_var raw) then () else failwith ("Type variable " ^ typ_var ^ " is not defined")

let alt_equal (alt1 : (string * string) list) (alt2 : (string * string) list) : bool =
  let rec contains (target : (string * string)) : (string * string) list -> bool = function
  | (t1, t2) :: ts -> (String.equal t1 (fst target)) && (String.equal t2 (snd target)) || contains target ts
  | [] -> false
  in
  let rec check (alt : (string * string) list) : (string * string) list -> bool = function 
  | target :: ts -> contains target alt && check alt ts
  | [] -> true
  in
  check alt1 alt2 && check alt2 alt1

let typ_equal (typ1 : typ) (typ2 : typ) : bool =
  match (typ1, typ2) with 
  | (Tensor (t1, t2), Tensor (t1', t2')) -> (String.equal t1 t1') && (String.equal t2 t2')
  | (Par (t1, t2), Par (t1', t2')) -> (String.equal t1 t1') && (String.equal t2 t2')
  | (Lolli (t1, t2), Lolli (t1', t2')) -> (String.equal t1 t1') && (String.equal t2 t2')
  | (One, One) -> true
  | (Plus alt1, Plus alt2) -> alt_equal alt1 alt2
  | (With alt1, With alt2) -> alt_equal alt1 alt2
  | _ -> false

let rec find_typ (typ : typ) : prog -> string option = function
  | d :: ds -> (
    match d with 
    | TypDef (str, typ') -> if typ_equal typ typ' then Some str else find_typ typ ds
    | _ -> find_typ typ ds
  )
  | [] -> None

let rec elab_typ (raw : E.prog) (env : prog) : E.typ -> (prog * typ) = function
  | E.Var str -> (
    let () = check_def_typ_env str env in 
    (env, expand_env str env)
  )
  | E.Tensor (t1, t2) -> 
    let (env1, t1') = name_typ raw env t1 in
    let (env2, t2') = name_typ raw env1 t2 in
    (env2, Tensor (t1', t2'))
  | E.Par (t1, t2) -> 
    let (env1, t1') = name_typ raw env t1 in
    let (env2, t2') = name_typ raw env1 t2 in
    (env2, Par (t1', t2'))
  | E.Plus alts -> 
    let (env', alts') = elab_alts raw env alts in 
    (env', Plus alts')
  | E.With alts -> 
    let (env', alts') = elab_alts raw env alts in 
    (env', With alts')
  | E.One -> (env, One) 
  | E.Lolli (t1, t2) -> 
    let (env1, t1') = name_typ raw env t1 in
    let (env2, t2') = name_typ raw env1 t2 in
    (env2, Lolli (t1', t2'))

and elab_alts (raw : E.prog) (env : prog) : ((string * E.typ) list -> (prog * (string * string) list)) = function
  | (str, typ) :: alts -> 
    let (env', typ') = name_typ raw env typ in 
    let (env'', alts') = elab_alts raw env' alts in 
    (env'', (str, typ') :: alts')
  | [] -> (env, [])  

and name_typ (raw : E.prog) (env : prog) : E.typ -> (prog * string) = function
  | E.Var str -> 
      let () = if is_def_typ_env str env then () else check_def_typ_raw str raw in
      (env, str)
  | typ -> 
    let (env', typ') = elab_typ raw env typ in (
      match find_typ typ' env' with 
      | Some str -> (env', str)
      | None -> 
        let str = freshName () in 
        (TypDef (str, typ') :: env', str)
    )


let elab_msg : E.msg -> msg = function
  | E.Unit -> Unit
  | E.Label str -> Label str
  | E.Channel str -> Channel (ChanVar str)

let rec elab_proc (raw : E.prog) (env : prog) : E.proc -> (prog * proc) = function
  | E.Send (str, msg, proc) -> 
    let msg' = elab_msg msg in (
      match proc with 
      | Some p -> 
        let (env', proc') = elab_proc raw env p in
        (env', Send (ChanVar str, msg', Some proc'))
      | None -> (env, Send (ChanVar str, msg', None))
    )
  | E.Recv (str, cont) -> 
    let (env', cont') = elab_cont raw env cont in 
    (env', Recv (ChanVar str, cont'))
  | E.Fwd (str1, str2) -> (env, Fwd (ChanVar str1, ChanVar str2))
  | E.Call (str, chans1, chans2, proc) -> 
    let chans1' = List.map ~f:(fun str -> ChanVar str) chans1 in
    let chans2' = List.map ~f:(fun str -> ChanVar str) chans2 in (
      match proc with
        | Some p -> 
          let (env', proc') = elab_proc raw env p in
          (env', Call (str, chans1', chans2', Some proc'))
        | None -> (env, Call (str, chans1', chans2', None))
    )
  | E.Cancel (str, proc) -> (
      match proc with 
      | Some p -> 
        let (env', proc') = elab_proc raw env p in
        (env', Cancel (ChanVar str, Some proc'))
      | None -> (env, Cancel (ChanVar str, None))
    )
  | E.Trycatch (proc1, proc2) -> 
    let (env1, proc1') = elab_proc raw env proc1 in
    let (env2, proc2') = elab_proc raw env1 proc2 in
    (env2, Trycatch (proc1', proc2'))
  | E.Raise proc -> 
    let (env', proc') = elab_proc raw env proc in
    (env', Raise proc')
  | E.Cut (str, typ, proc1, proc2) -> 
    let (env1, proc1') = elab_proc raw env proc1 in
    let (env2, proc2') = elab_proc raw env1 proc2 in (
      match typ with
      | Some t -> 
        let (env3, t') = elab_typ raw env2 t in
        (env3, Cut (ChanVar str, Some t', proc1', proc2'))
      | None -> (env2, Cut (ChanVar str, None, proc1', proc2'))
    )

and elab_cont (raw : E.prog) (env : prog) : E.cont -> (prog * cont) = function
  | E.ContUnit proc -> 
    let (env', proc') = elab_proc raw env proc in
    (env', ContUnit proc')
  | E.ContLabel branches -> 
    let (env', branches') = elab_branches raw env branches in
    (env', ContLabel branches')
  | E.ContChannel (str, proc) -> 
    let (env', proc') = elab_proc raw env proc in
    (env', ContChannel (ChanVar str, proc'))

and elab_branches (raw : E.prog) (env : prog) : (string * E.proc) list -> (prog * (string * proc) list) = function
  | (str, proc) :: bs ->
    let (env', proc') = elab_proc raw env proc in
    let (env'', bs') = elab_branches raw env' bs in
    (env'', (str, proc') :: bs')
  | [] -> (env, []) 

let rec elab_parms (raw : E.prog) (env : prog) : (string * E.typ) list -> (prog * (channel * typ) list) = function
  | (str, typ) :: ps -> 
    let (env', typ') = elab_typ raw env typ in 
    let (env'', ps') = elab_parms raw env' ps in 
    (env'', (ChanVar str, typ') :: ps')
  | [] -> (env, [])

let rec elab_prog (raw : E.prog) (env : prog) : E.prog -> prog = function
  | E.TypDef (str, typ) :: ds -> 
    let (env', typ') = elab_typ raw env typ in 
    elab_prog raw (TypDef (str, typ') :: env') ds
  | E.ProcDef (str, parms1, parms2, proc) :: ds -> 
    let (env', parms1') = elab_parms raw env parms1 in 
    let (env'', parms2') = elab_parms raw env' parms2 in 
    let (env''', proc') = elab_proc raw env'' proc in 
    elab_prog raw (ProcDef (str, parms1', parms2', proc') :: env''') ds
  | E.ExnProcDef (str, parms1, parms2, proc) :: ds -> 
    let (env', parms1') = elab_parms raw env parms1 in 
    let (env'', parms2') = elab_parms raw env' parms2 in 
    let (env''', proc') = elab_proc raw env'' proc in 
    elab_prog raw (ExnProcDef (str, parms1', parms2', proc') :: env''') ds
  | E.Exec str :: ds -> elab_prog raw (Exec str :: env) ds
  | [] -> List.rev env

let elab (raw : E.prog) : prog = elab_prog raw [] raw

module Print = struct

  let pp_typ : typ -> string = function
    | Tensor (t1, t2) -> t1 ^ " * " ^ t2
    | Par (t1, t2) -> t1 ^ " @ " ^ t2
    | Plus alts -> 
      let alts' = List.map ~f:(fun (s, t) -> s ^ " : " ^ t) alts in 
      "{" ^ (String.concat ~sep:" + " alts') ^ "}"
    | With alts -> 
      let alts' = List.map ~f:(fun (s, t) -> s ^ " : " ^ t) alts in 
      "{" ^ (String.concat ~sep:" & " alts') ^ "}"
    | One -> "1"
    | Lolli (t1, t2) -> t1 ^ " -o " ^ t2

  let pp_channel : channel -> string = function
    | ChanVar str -> str
    | ChanConst i -> "#" ^ (Int.to_string i)

  let pp_msg : msg -> string = function
    | Unit -> "()"
    | Label str -> str
    | Channel c -> pp_channel c
  
  let rec pp_proc : proc -> string = function
    | Send (c, m, optp) -> sprintf "send %s %s%s" (pp_channel c) (pp_msg m) (
        match optp with
        | None -> ""
        | Some p -> "; " ^ pp_proc p
      )
    | Recv (c, k) -> sprintf "recv %s (%s)" (pp_channel c) (pp_cont k)
    | Fwd (c1, c2) -> sprintf "fwd %s %s" (pp_channel c1) (pp_channel c2)
    | Call (f, xs, ys, optp) -> sprintf "call %s (%s) [%s]%s" f 
      (String.concat ~sep:", " (List.map ~f:pp_channel xs)) 
      (String.concat ~sep:", " (List.map ~f:pp_channel ys)) (
        match optp with
        | None -> ""
        | Some p -> "; " ^ pp_proc p
      )
    | Cancel (c, optp) -> sprintf "cancel %s%s" (pp_channel c) (
        match optp with
        | None -> ""
        | Some p -> "; " ^ pp_proc p
    )
    | Trycatch (p1, p2) -> sprintf "try (%s) catch (%s)" (pp_proc p1) (pp_proc p2)
    | Raise p -> sprintf "raise (%s)" (pp_proc p)
    | Cut (c, optt, p1, p2) -> sprintf "%s <- (%s); %s" (
        match optt with
        | None -> (pp_channel c)
        | Some t -> sprintf "(%s:%s)" (pp_channel c) (pp_typ t)
      ) (pp_proc p1) (pp_proc p2)

  and pp_cont : cont -> string = function
    | ContUnit p -> sprintf "() => %s" (pp_proc p)
    | ContLabel ks -> let ks' = List.map ~f:(fun (s, p) -> s ^ " => " ^ (pp_proc p)) ks in 
      (String.concat ~sep:" | " ks')
    | ContChannel (c, p) -> sprintf "%s => %s" (pp_channel c) (pp_proc p)
  
  let pp_def : def -> string = function
    | TypDef (str, typ) -> sprintf "type %s = %s" str (pp_typ typ)
    | ProcDef (f, xs, ys, p) -> 
      let xs' = List.map ~f:(fun (c, t) -> (pp_channel c) ^ " : " ^ (pp_typ t)) xs in 
      let ys' = List.map ~f:(fun (c, t) -> (pp_channel c) ^ " : " ^ (pp_typ t)) ys in
        sprintf "proc %s (%s) [%s] = %s" f (String.concat ~sep:", " xs') (String.concat ~sep:", " ys') (pp_proc p)
    | ExnProcDef (f, xs, ys, p) ->
      let xs' = List.map ~f:(fun (c, t) -> (pp_channel c) ^ " : " ^ (pp_typ t)) xs in 
      let ys' = List.map ~f:(fun (c, t) -> (pp_channel c) ^ " : " ^ (pp_typ t)) ys in
        sprintf "exnproc %s (%s) [%s] = %s" f (String.concat ~sep:", " xs') (String.concat ~sep:", " ys') (pp_proc p)
    | Exec str -> sprintf "exec %s" str

    
  let rec pp_prog = function
    | [] -> ""
    | d :: ds -> (pp_def d) ^ "\n" ^ (pp_prog ds)


end