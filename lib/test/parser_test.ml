
open Core

module E = Extsyn

let parse (s : string) : E.prog =
  let lexbuf = Lexing.from_string s in
    Parser.prog Lexer.read lexbuf

let%expect_test "Test parsing" =
  let program =
      "type bool = +{'true : 1, 'false : 1}"
  in
  let ast = parse program in
  print_endline (E.Print.pp_prog ast);
  [%expect]
;;
