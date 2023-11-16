# ePass
## Grammer for ePass
```
<tp> ::= <id>
       | '1'
       | '+' '{' <alts> '}'
       | '&' '{' <alts> '}'
       | <tp> '|' <tp>
       | <tp> '*' <tp>
       | <tp> '-o' <tp>
       | '(' <tp> ')'

<alts> ::= <tag> ':' <tp> [ ',' <alts> ]

<parm> ::= '(' <id> ':' <tp> ')'

<proc> ::= 'send' <id> <msg> [ ';' <proc> ]
         | 'recv' <id> '(' <cont> ')'
         | 'fwd' <id> <id>
         | 'call' <id> '(' <args> ')' '[' <args> ']' [ ';' <proc> ]
         | 'cancel' <id>
         | 'try' <proc> 'catch' <proc>
         | 'raise' <proc>
         | <id> [ ':' <tp> ] '<-' <proc> ';' <proc>
         | '(' <proc> ')'

<args> ::= <id> [ ',' <args> ]

<cont> ::= <msg> '=>' <proc> [ '|' <cont> ]

<msg> ::= '(' ')'
        | <tag>
        | <id>

<parms> ::= <id> : <tp> [, <parms>]

<defn> ::= 'type' <id> = <tp>
         | 'proc' <id> '(' <parms> ')' '[' <parms> ']' '=' <proc>

<prog> ::= <defn>*
```