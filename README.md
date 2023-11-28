# ePass Documentation
## Grammer for ePass
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

<annot> ::= · | ':' <tp>

<procfollow> ::= · | ';' <proc>

<simpleproc> ::= 'send' <id> <msg>
         | 'recv' <id> '(' <cont> ')'
         | 'fwd' <id> <id>
         | 'call' <id> '(' <args> ')' '[' <args> ']' 
         | 'cancel' <id>

<proc> ::= 'send' <id> <msg> <procfollow>
         | 'recv' <id> '(' <cont> ')'
         | 'fwd' <id> <id>
         | 'call' <id> '(' <args> ')' '[' <args> ']' <procfollow>
         | 'cancel' <id> <procfollow>
         | 'try' <proc> 'catch' <proc>
         | 'raise' <proc>
         | <id> <annot> '<-' <simpleproc> ';' <proc>
         | <id> <annot> '<-' '(' <proc> ')' ';' <proc>
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

## execute ePass
First build the project
```
dune build
```
Create `<filename>.eps` in you repository, and run the file.
```
dune exec ePass <filename>.eps
```