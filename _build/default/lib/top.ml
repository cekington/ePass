open Core

module E = Extsyn

let main () = 
  let parse (s : string) : E.prog =
    let () = print_string s in
    let lexbuf = Lexing.from_string s in
      Parser.prog Lexer.read lexbuf
  in 
    print_string(E.Print.pp_prog (parse "type bool = +{'true : 1, 'false : 1}"))
