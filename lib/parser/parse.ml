module E = Extsyn

let parse (s : string) : E.prog =
  let lexbuf = Lexing.from_string s in
    Parser.prog Lexer.read lexbuf