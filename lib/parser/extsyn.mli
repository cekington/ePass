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
  | Call of string * string list * string list * proc option
  | Cancel of string * proc option
  | Trycatch of proc * proc
  | Raise of proc 
  | Cut of string * typ option * proc * proc

and cont = 
  | Cont of (msg * proc) list

type def = 
  | TypDef of string * typ
  | ProcDef of string * (string * typ) list * (string * typ) list * proc

type prog = def list

module Print : sig

  val pp_typ : typ -> string
  val pp_msg : msg -> string
  val pp_proc : proc -> string
  val pp_cont : cont -> string
  val pp_def : def -> string
  val pp_prog : prog -> string

end