open Core

type typ = 
  | TypName of string
  | Tensor of typ * typ
  | Plus of (string * typ) list
  | With of (string * typ) list
  | One 
  | Lolli of typ * typ

module Print = struct
  let rec pp_typ = function
  | TypName s -> s
  | Tensor (t1, t2) -> "(" ^ (pp_typ t1) ^ " * " ^ (pp_typ t2) ^ ")"
  | Plus ts -> let ts' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) ts in 
    "(" ^ (String.concat ~sep:" + " ts') ^ ")"
  | With ts -> let ts' = List.map ~f:(fun (s, t) -> s ^ " : " ^ (pp_typ t)) ts in 
  "(" ^ (String.concat ~sep:" & " ts') ^ ")"
  | One -> "1"
  | Lolli (t1, t2) -> "(" ^ (pp_typ t1) ^ " -o " ^ (pp_typ t2) ^ ")"
end