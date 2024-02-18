open Core

type typ = 
  | Var of string
  | Tensor of typ * typ
  | Par of typ * typ
  | Plus of (string * typ) list
  | With of (string * typ) list
  | One 
  | Lolli of typ * typ

type msg = 
  | Unit
  | Label of string
  | Channel of string

type proc = 
  | Send of string * msg * proc option
  | Recv of string * cont 
  | Fwd of string * string
  | Call of string * string list * string list
  | Cancel of string * proc option
  | Trycatch of string * typ * proc * proc
  | Raise of proc 
  | Cut of string * typ * proc * proc

and cont = 
  | ContUnit of proc 
  | ContLabel of (string * proc) list
  | ContChannel of string * proc

type def = 
  | TypDef of string * typ
  | ProcDef of string * (string * typ) list * (string * typ) list * proc
  | ExnProcDef of string * (string * typ) list * (string * typ) list * proc
  | Exec of string 

type prog = def list
module Print = struct

  let rec pp_typ = function
  | Var v -> v
  | Tensor (t1, t2) -> sprintf "(%s * %s)" (pp_typ t1) (pp_typ t2)
  | Par (t1, t2) -> sprintf "(%s | %s)" (pp_typ t1) (pp_typ t2)
  | Plus ts -> let ts' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) ts in 
    "{" ^ (String.concat ~sep:" + " ts') ^ "}"
  | With ts -> let ts' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) ts in 
  "{" ^ (String.concat ~sep:" & " ts') ^ "}"
  | One -> "1"
  | Lolli (t1, t2) -> sprintf "%s -o %s" (pp_typ t1) (pp_typ t2)

  let pp_msg = function
  | Label s -> s
  | Unit -> "()"
  | Channel c -> c

  let rec pp_proc = function
  | Send (c, m, optp) -> sprintf "send %s %s%s" c (pp_msg m) (
      match optp with
      | None -> ""
      | Some p -> "; " ^ pp_proc p
    )
  | Recv (c, k) -> sprintf "recv %s (%s)" c (pp_cont k)
  | Fwd (c1, c2) -> sprintf "fwd %s %s" c1 c2
  | Call (f, xs, ys) -> sprintf "call %s (%s) [%s]" f (String.concat ~sep:", " xs) (String.concat ~sep:", " ys)
  | Cancel (c, optp) -> sprintf "cancel %s%s" c (
      match optp with
      | None -> ""
      | Some p -> "; " ^ pp_proc p
  )
  | Trycatch (c, t, p1, p2) -> sprintf "%s : %s <<- (%s) catch (%s)" c (pp_typ t) (pp_proc p1) (pp_proc p2)
  | Raise p -> sprintf "raise (%s)" (pp_proc p)
  | Cut (c, t, p1, p2) -> sprintf "%s : %s <- (%s); %s"  c (pp_typ t)
    (pp_proc p1) (pp_proc p2)

  and pp_cont = function
  | ContUnit p -> sprintf "() => %s" (pp_proc p)
  | ContLabel ks -> let ks' = List.map ~f:(fun (s, p) -> s ^ " => " ^ (pp_proc p)) ks in 
    (String.concat ~sep:" | " ks')
  | ContChannel (c, p) -> sprintf "%s => %s" c (pp_proc p)

  let pp_def = function
  | TypDef (s, t) -> sprintf "type %s = %s" s (pp_typ t)
  | ProcDef (f, xs, ys, p) -> 
    let xs' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) xs in 
    let ys' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) ys in
      sprintf "proc %s (%s) [%s] = %s" f (String.concat ~sep:", " xs') (String.concat ~sep:", " ys') (pp_proc p)
  | ExnProcDef (f, xs, ys, p) ->
    let xs' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) xs in 
    let ys' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) ys in
      sprintf "exnproc %s (%s) [%s] = %s" f (String.concat ~sep:", " xs') (String.concat ~sep:", " ys') (pp_proc p)
  | Exec p -> sprintf "exec %s" p

  let rec pp_prog = function
  | [] -> ""
  | d :: ds -> (pp_def d) ^ "\n" ^ (pp_prog ds)

end