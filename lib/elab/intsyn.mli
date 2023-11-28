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

val elab : E.prog -> prog

module Print : sig

    val pp_typ : typ -> string
    val pp_channel : channel -> string
    val pp_msg : msg -> string
    val pp_proc : proc -> string
    val pp_cont : cont -> string
    val pp_def : def -> string
    val pp_prog : prog -> string
  
  end