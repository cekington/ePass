%{
open Extsyn
%}

%token <string> ID
%token <string> TAG

%token ONE 
%token PLUS
%token WITH
%token TIMES
%token LOLLI
%token PAR
%token EQUAL

%token SEMICOLON
%token COLON
%token COMMA

%token SEND
%token RECV
%token FWD
%token CALL
%token CANCEL
%token TRY
%token CATCH
%token RAISE
%token SLARROW
%token BAR
%token BRARROW

%token TYPE 
%token PROC 

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE

%token EOF

%type <typ> typ
%type <(string * typ) list> altsfollow
%type <(string * typ) list> alts
%type <typ option> annot
%type <proc> simpleproc
%type <proc option> procfollow
%type <proc> proc
%type <string list> argsfollow
%type <string list> args
%type <(string * typ) list> annoargsfollow
%type <(string * typ) list> annoargs
%type <(string * proc) list> contfollow
%type <cont> cont
%type <msg> msg
%type <def> defn

%start <prog> prog

%right LOLLI
%right TIMES
%right PAR

%%

typ :
  | t = ID;
    { Var t }
  | ONE;
    { One }
  | PLUS; LBRACE; l = alts; RBRACE;
    { Plus l }
  | WITH; LBRACE; l = alts; RBRACE;
    { With l }
  | t1 = typ; PAR; t2 = typ;
    { Par (t1, t2) }
  | t1 = typ; TIMES; t2 = typ;
    { Tensor (t1, t2) }
  | t1 = typ; LOLLI; t2 = typ;
    { Lolli (t1, t2) }
  | LPAREN; t = typ; RPAREN;
    { t }
  ;

altsfollow :
  | { [] }
  | COMMA; l = TAG; COLON; t = typ; a = altsfollow;
    { (l, t) :: a }
  ;

alts :
  | l = TAG; COLON; t = typ; a = altsfollow;
    { (l, t) :: a }
  ;

annot :
  | { None }
  | COLON; t = typ;
    { Some t }
  ; 

procfollow :
  | { None }
  | SEMICOLON; p = proc;
    { Some p }
  ;

simpleproc : 
  | SEND; a = ID; m = msg;
    { Send (a, m, None) }
  | RECV; a = ID; LPAREN; c = cont; RPAREN;
    { Recv (a, c) }
  | FWD; a = ID; b = ID;
    { Fwd (a, b) }
  | CALL; p = ID; LPAREN; provides = args; RPAREN; LBRACKET; uses = args; RBRACKET;
    { Call (p, provides, uses, None) }
  | CANCEL; a = ID;
    { Cancel (a, None) }

proc :  
  | SEND; a = ID; m = msg; pf = procfollow
    { Send (a, m, pf) }
  | RECV; a = ID; LPAREN; c = cont; RPAREN;
    { Recv (a, c) }
  | FWD; a = ID; b = ID;
    { Fwd (a, b) }
  | CALL; p = ID; LPAREN; provides = args; RPAREN; LBRACKET; uses = args; RBRACKET; pf = procfollow;
    { Call (p, provides, uses, pf) }
  | CANCEL; a = ID; pf = procfollow;
    { Cancel (a, pf) }
  | TRY; p = proc; CATCH; q = proc;
    { Trycatch (p, q) }
  | RAISE; p = proc;
    { Raise p }
  | x = ID; t = annot; SLARROW; p = simpleproc; SEMICOLON; q = proc
    { Cut (x, t, p, q) }
  | x = ID; t = annot; SLARROW; LPAREN; p = proc; RPAREN; SEMICOLON; q = proc
    { Cut (x, t, p, q) }
  | LPAREN; p = proc; RPAREN;
    { p }
  ;

argsfollow :
  | { [] }
  | COMMA; id = ID; a = argsfollow;
    { id :: a }
  ;

args :
  | { [] }
  | id = ID; a = argsfollow;
    { id :: a }
  ;

annoargsfollow :
  | { [] }
  | COMMA; x = ID; COLON; t = typ; a = annoargsfollow
    { (x, t) :: a }
  ;

annoargs : 
  | { [] }
  | x = ID; COLON; t = typ; a = annoargsfollow
    { (x, t) :: a }
  ;

contfollow :
  | { [] }
  | BAR; m = TAG; BRARROW; p = proc; c = contfollow
    { (m, p) :: c }
  ;

cont : 
  | LPAREN; RPAREN; BRARROW; p = proc;
    { ContUnit p }
  | x = ID;  BRARROW; p = proc;
    { ContChannel (x, p) }
  | m = TAG; BRARROW; p = proc; c = contfollow
    { ContLabel ((m, p) :: c) }
  ;

msg : 
  | LPAREN; RPAREN
    { Unit }
  | l = TAG
    { Label l }
  | x = ID;
    { Channel x }   
  ; 

defn : 
  | TYPE; id = ID; EQUAL; t = typ;
    { TypDef (id, t) }
  | PROC; id = ID; LPAREN; provides = annoargs; RPAREN; LBRACKET; uses = annoargs; RBRACKET; EQUAL; p = proc;
    { ProcDef (id, provides, uses, p) }
  ;

prog :
  | EOF;
    { [] }
  | d = defn; p = prog; EOF;
    { d :: p }
  ;