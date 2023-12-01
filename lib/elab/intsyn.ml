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