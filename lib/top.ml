open Core

module E = Extsyn
module I = Intsyn

let main () = 
  let filename = (Sys.get_argv()).(1) in
  let fileContent = String.concat (In_channel.read_lines filename) in
  let progE : E.prog = Parse.parse fileContent in
  let progI : I.prog = I.elab progE in
  print_string(I.Print.pp_prog progI)
