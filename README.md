# ePass Documentation
## Grammer
```
<idchar> ::= [a-zA-Z_0-9']*
<id> ::= [a-zA-Z_]<idchar>
<tag> ::= '<idchar>+

<tp> ::= <id>
       | '1'
       | '+' '{' <alts> '}'
       | '&' '{' <alts> '}'
       | <tp> '@' <tp>
       | <tp> '*' <tp>
       | <tp> '-o' <tp>
       | '(' <tp> ')'

<altsfollow> ::= · | ',' <tag> ':' <tp> <altsfollow>

<alts> ::= <tag> ':' <tp> <altsfollow>

<procfollow> ::= · | ';' <proc>

<simpleproc> ::= 'send' <id> <msg>
         | 'recv' <id> '(' <cont> ')'
         | 'fwd' <id> <id>
         | 'call' <id> '(' <args> ')' '[' <args> ']' 
         | 'cancel' <id>

<proc> ::= 'send' <id> <msg> <procfollow>
         | 'recv' <id> '(' <cont> ')'
         | 'fwd' <id> <id>
         | 'call' <id> '(' <args> ')' '[' <args> ']'
         | 'cancel' <id> <procfollow>
         | <id> ':' <tp> '<<-' <proc> 'catch' <proc>
         | 'raise' <proc>
         | <id> ':' <tp> '<-' <simpleproc> ';' <proc>
         | <id> ':' <tp> '<-' '(' <proc> ')' ';' <proc>
         | '(' <proc> ')'

<argsfollow> ::= · | ',' <id> <argsfollow>

<args> ::= <id> <argsfollow>

<annoargsfollow> ::= · | ',' <id> ':' <type> <annoargsfollow>

<annoargs> ::= <id> ':' <type> <annoargsfollow>

<contfollow> ::= · | '|' <msg> '=>' <proc> <contfollow>

<cont> ::= <msg> '=>' <proc> <contfollow>

<msg> ::= '(' ')'
        | <tag>
        | <id>

<defn> ::= 'type' <id> = <tp>
         | 'proc' <id> '(' <annoargs> ')' '[' <annoargs> ']' '=' <proc>
         | 'exnproc' <id> '(' <annoargs> ')' '[' <annoargs> ']' '=' <proc>
         | 'exec' <id>

<prog> ::= · | <defn> <prog>
```

## Execute ePass
First build the project
```
dune build
```
Create `<filename>.eps` in you repository, and run the file.
```
dune exec ePass <filename>.eps
```

## Interpreter phrase
### Parsing
The Menhir parser is used to parse file input into external syntax.

### Elaboration
Elaborate the external syntax into internal syntax by introducing internal type variables to eliminate recursive type declaration. Meanwhile, check proper type, process and execution definition.

### TypeChecking 
Type checking the program, may raise various type checking error.

### Runtime
Simulate concurrent execution and make observation by printing into the console.