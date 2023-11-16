# ePass
## Grammer for ePass
```
<tp> ::= <id>
       | '1'
       | '+' '{' <alts> '}'
       | '&' '{' <alts> '}'
       | <tp> '*' <tp>
       | <tp> '-o' <tp>
       | '(' <tp> ')'

<alts> ::= <tag> ':' <tp> [ ',' <alts> ]

<parm> ::= '(' <id> ':' <tp> ')'

<proc> ::= 'send' <id> <msg> [ ';' <proc> ]
         | 'recv' <id> '(' <match> ')'
         | 'fwd' <id> <id>
         | 'call' <id> '(' <args> ')' '[' <args> ']' [ ';' <proc> ]
         | 'cancel' <id>
         | 'try' <proc> 'catch' <proc>
         | 'raise' <proc>
         | <id> [ ':' <tp> ] '<-' <proc> ';' <proc>
         | '(' <proc> ')'

<args> ::= <id> [ ',' <args> ]

<match> ::= <msg> '=>' <proc> [ '|' <match> ]

<msg> ::= '(' ')'
        | <tag>
        | <id>

<defn> ::= 'type' <id> = <tp>
         | 'proc' <id> <parm> <parms>* = <proc>
         | 'exec' <id>   % requires empty antecedent
         | 'fail' <defn>

<prog> ::= <defn>*
```