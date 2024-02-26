open Core

module E = Extsyn
module I = Intsyn
module S = Statics
module D = Dynamics

let main () = 
  let filename = (Sys.get_argv()).(1) in
  let fileContent = String.concat ~sep:"\n" (In_channel.read_lines filename) in
  try 
    let progE : E.prog = Parse.parse fileContent in
    try 
      let progI : I.prog = Elab.elab progE in
      let () = S.typecheck progI in
      try 
        let output = D.exec progI in
        print_string(output)
      with Failure e ->
        print_string("Runtime Error: " ^ e ^ "\n")
    with Failure e ->
      print_string("Typecheck Error: " ^ e ^ "\n")
  with _e ->
    print_string("Parse Error\n") 
