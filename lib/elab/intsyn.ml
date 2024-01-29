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
  | Dual of string

type msg = 
  | Unit
  | Label of string
  | Channel of channel

type proc = 
  | Send of channel * msg * proc option
  | Recv of channel * cont 
  | Fwd of channel * channel
  | Call of string * channel list * channel list
  | Cancel of channel * proc option
  | Trycatch of proc * proc
  | Raise of proc 
  | Cut of channel * typ * proc * proc
  | Null

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

type position = 
  | Antecedent
  | Succedent

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
  | (Dual str1, Dual str2) -> String.equal str1 str2
  | (One, One) -> true
  | (Plus alt1, Plus alt2) -> alt_equal alt1 alt2
  | (With alt1, With alt2) -> alt_equal alt1 alt2
  | _ -> false

let channel_equal (c1 : channel) (c2 : channel) : bool =
  match (c1, c2) with 
  | (ChanVar str1, ChanVar str2) -> String.equal str1 str2
  | (ChanConst i1, ChanConst i2) -> Int.equal i1 i2
  | _ -> false

let rec expand_env (typ_var : string) : prog -> typ = function
  | d :: ds -> (
    match d with 
    | TypDef (str, typ) -> if String.equal str typ_var then typ else expand_env typ_var ds
    | _ -> expand_env typ_var ds
  )
  | [] -> failwith "expand_env raise Impossible error"

let find_proc (env : prog) (proc_name : string) : def =
  let result = List.find ~f:(function 
    | ProcDef (f, _, _, _) -> String.equal f proc_name
    | ExnProcDef (f, _, _, _) -> String.equal f proc_name
    | _ -> false
  ) env in 
  match result with
  | Some def -> def
  | None -> failwith "find_proc raise Impossible error"

let rec subst_channel (gamma : (channel * channel) list) (c : channel) : channel = 
  match gamma with 
  | [] -> c 
  | (c1, c2) :: gs -> if channel_equal c c2 then c1 else subst_channel gs c

let subst_msg (gamma : (channel * channel) list) : msg -> msg = function 
  | Unit -> Unit 
  | Label str -> Label str
  | Channel c -> Channel (subst_channel gamma c) 

(* [a/x]P *)
let rec subst_proc (gamma : (channel * channel) list) : proc -> proc = function 
  | Send (c, m, oproc) -> 
    Send (subst_channel gamma c, subst_msg gamma m, Option.map ~f:(subst_proc gamma) oproc)
  | Recv (c, k) -> Recv (subst_channel gamma c, subst_cont gamma k)
  | Fwd (c1, c2) -> Fwd (subst_channel gamma c1, subst_channel gamma c2)
  | Call (f, xs, ys) -> Call (f, List.map ~f:(subst_channel gamma) xs, List.map ~f:(subst_channel gamma) ys)
  | Cancel (c, oproc) -> Cancel (subst_channel gamma c, Option.map ~f:(subst_proc gamma) oproc)
  | Trycatch (p1, p2) -> Trycatch (subst_proc gamma p1, subst_proc gamma p2)
  | Raise p -> Raise (subst_proc gamma p)
  | Cut (c, t, p1, p2) -> Cut (c, t, subst_proc gamma p1, subst_proc gamma p2)
  | Null -> Null

and subst_cont (gamma : (channel * channel) list) : cont -> cont = function 
  | ContUnit p -> ContUnit (subst_proc gamma p)
  | ContLabel ks -> ContLabel (List.map ~f:(fun (s, p) -> (s, subst_proc gamma p)) ks)
  | ContChannel (c, p) -> ContChannel (c, subst_proc gamma p)

module Print = struct

  let pp_position : position -> string = function
    | Antecedent -> "antecedent"
    | Succedent -> "succedent"

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
    | Dual t -> t ^ "^" 

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
    | Call (f, xs, ys) -> sprintf "call %s (%s) [%s]" f 
      (String.concat ~sep:", " (List.map ~f:pp_channel xs)) 
      (String.concat ~sep:", " (List.map ~f:pp_channel ys))
    | Cancel (c, optp) -> sprintf "cancel %s%s" (pp_channel c) (
        match optp with
        | None -> ""
        | Some p -> "; " ^ pp_proc p
    )
    | Trycatch (p1, p2) -> sprintf "try (%s) catch (%s)" (pp_proc p1) (pp_proc p2)
    | Raise p -> sprintf "raise (%s)" (pp_proc p)
    | Cut (c, t, p1, p2) -> sprintf "%s : %s <- (%s); %s" (pp_channel c) (pp_typ t) (pp_proc p1) (pp_proc p2)
    | Null -> " "

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