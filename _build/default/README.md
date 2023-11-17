# ePass
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

<proc> ::= 'send' <id> <msg> <procfollow>
         | 'recv' <id> '(' <cont> ')'
         | 'fwd' <id> <id>
         | 'call' <id> '(' <args> ')' '[' <args> ']' <procfollow>
         | 'cancel' <id>
         | 'try' <proc> 'catch' <proc>
         | 'raise' <proc>
         | <id> <annot> '<-' <proc> ';' <proc>
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

<prog> ::= · | <defn> <prog>
```