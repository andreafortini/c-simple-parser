# C Simple Parser

The project aims to implement simple parser for sub-language of C with pattern-matching technique. The parser should recognize:
* Expression
* Assignement 
* Int variable declaration
* Selection
* Block {}
* Loops

It produces an Abstract Syntax Tree (AST) as output.

## Usage

The program should be compile with ghc and the executable takes two argument: the first one is the input file (.txt) and the second is the output file where the program will print the AST.

```bash
$ ghc c-parser.hs
$ ./c-parser input.txt output.txt
```

The input file shouldn't have main, but it should be wrapped between curly brackets.
Check the input file 'input.txt' and try to run the program to see how it works.


## Grammar

```EBNF
<program> ::= <compound>

<compound> ::= "{" <declaration_list> <statement_list> "}"

<declaration_list> ::= {<declaration>}

<declaration> ::= "int" string ";"

<statement_list> ::= {<statement>}

<statement> ::= <if_statement>
 |	<while_statement>
 |  <do_statement>
 |  <for_statement>
 |  <assignment_statement>
 |  <compound>

<if_statement> ::= "if" "(" <expression> ")" <statement> [<opt_else>]

<opt_else> ::= "else" <statement>

<while_statement> ::= "while" "(" <expression> ")" <statement>

<do_statement> ::= "do" "{" <statement> "}" "while" "(" <expression> ")" ";"

<for_statement> ::= "for" "(" <assignment_statement> ";" <expression> ";" <expression> ")" <statement>

<assignment_statement> ::= string "=" <expression>

<expression> ::= <equality>

<equality> ::= <comparison> { ("!="|"==") <comparison> }

<comparison> ::= <term> { (">"|">="|"<"|"<=") <term> }

<term> ::= <factor> { ("-"|"+") <factor> }

<factor> ::= <unary> { ("/"|"*") <unary> }

<unary> ::= ("!"|"-") <unary> | <primary>

<primary> ::= number | string | "(" <expression> ")"
```