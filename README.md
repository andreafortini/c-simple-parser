# C Simple Parser

The project aims to implement simple parser for sub-language of C. The parser can recognize:
* Expression
* Assignement 
* Int variable declaration
* Selection
* Block {}
* Loop

The parser produces an Abstract Syntax Tree (AST) as output.

## Grammar

expression
	: assignment_expression
	| expression ',' assignment_expression
	;
assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;
