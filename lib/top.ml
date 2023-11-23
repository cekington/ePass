open Core

module E = Extsyn

let parse (s : string) : E.prog =
  let lexbuf = Lexing.from_string s in
    Parser.prog Lexer.read lexbuf

let main () = 
  let filename = (Sys.get_argv()).(1) in
  let fileContent = String.concat (In_channel.read_lines filename) in
  let prog = parse fileContent in
  print_string(E.Print.pp_prog prog)
