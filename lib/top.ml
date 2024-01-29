open Core

module E = Extsyn
module I = Intsyn
module S = Statics
module D = Dynamics

let main () = 
  let filename = (Sys.get_argv()).(1) in
  let fileContent = String.concat ~sep:"\n" (In_channel.read_lines filename) in
  let progE : E.prog = Parse.parse fileContent in
  let progI : I.prog = Elab.elab progE in
  let () = S.typecheck progI progI in
  D.exec progI
