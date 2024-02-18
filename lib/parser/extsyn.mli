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

module Print : sig

  val pp_typ : typ -> string
  val pp_msg : msg -> string
  val pp_proc : proc -> string
  val pp_cont : cont -> string
  val pp_def : def -> string
  val pp_prog : prog -> string

end