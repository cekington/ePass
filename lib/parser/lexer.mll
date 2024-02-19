{
open Parser
}

let white = [' ' '\t']+
let idchar = ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']
let id = ['a' - 'z' 'A' - 'Z']idchar*
let tag = [''']idchar+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | eof { EOF }
  | "1" { ONE }
  | "+" { PLUS }
  | "&" { WITH }
  | "*" { TIMES }
  | "-o" { LOLLI }
  | "@" { PAR }
  | "=" { EQUAL }
  | ";" { SEMICOLON }
  | ":" { COLON }
  | "," { COMMA }
  | "send" { SEND }
  | "recv" { RECV }
  | "fwd" { FWD }
  | "call" { CALL }
  | "cancel" { CANCEL }
  | "<<-" { SLLARROW }
  | "try" { TRY }
  | "catch" { CATCH }
  | "raise" { RAISE }
  | "<-" { SLARROW }
  | "=>" { BRARROW }
  | "|" { BAR }
  | "type" { TYPE }
  | "proc" { PROC }
  | "exnproc" { EXNPROC }
  | "exec" { EXEC }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | id { ID (Lexing.lexeme lexbuf) }
  | tag { TAG (Lexing.lexeme lexbuf) }
