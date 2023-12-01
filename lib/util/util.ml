open Core

let counter = ref 0

let reset : unit -> unit = fun () -> counter := 0

let freshName () =
  let name = "%tp_" ^ (string_of_int !counter) in
  counter := !counter + 1;
  name