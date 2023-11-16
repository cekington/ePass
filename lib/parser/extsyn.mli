type typ = 
  | TypName of string
  | Tensor of typ * typ
  | Plus of (string * typ) list
  | With of (string * typ) list
  | One 
  | Lolli of typ * typ

module Print : sig
  val pp_typ : typ -> string
end