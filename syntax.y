%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <errno.h>
	#include <math.h>
	#include <stdbool.h>
	#include "syntax.tab.h"
    
	extern FILE *yyin;
	extern int yylex();
	extern void yyerror(const char *err);
%}

%define parse.error verbose

%union	{
	int 		intval;
	float		floatval;
	long 		lval;
	long double	rval;
	char		charval;
}

//  optional	required	optional	optional
// %token	<type>		<name>		<number>	"description"
%token	T_FUNCTION		"function"
%token	T_SUBROUTINE	"subroutine" 
%token	T_END			"end"
%token	T_INTEGER		"int"
%token	T_REAL			"real"
%token	T_LOGICAL 		"logical"
%token	T_CHARACTER 	"char"
%token	T_COMPLEX 		"complex"
%token	T_RECORD		"record"
%token	T_ENDREC		"endrec"
%token	T_LIST			"list"
%token	T_DATA			"data"
%token	T_CONTINUE		"continue"
%token	T_GOTO			"goto"
%token	T_CALL			"call"
%token	T_READ			"read"
%token	T_WRITE			"write"
%token	T_NEW			"new"
%token	T_LENGTH		"length"
%token	T_IF			"if" 
%token	T_THEN			"then" 
%token	T_ELSE			"else"
%token	T_ENDIF			"endif"
%token	T_DO			"do"
%token	T_ENDDO			"enddo"
%token	T_STOP			"stop"
%token	T_RETURN		"return"
	
// ID
%token	T_ID	"id"

// Constants
%token	<intval>	T_ICONST	"iconst"
%token	<rval>		T_RCONST	"rconst"
%token	<lval>		T_LCONST	"lconst"
%token	<charval>	T_CCONST	"cconst"

// Operators
%token	T_OROP		"orop"
%token	T_ANDOP		"andop"
%token	T_NOTOP		"notop"
%token	T_RELOP 	"relop"
%token	T_ADDOP		"addop"
%token	T_MULOP		"mulop"
%token	T_DIVOP		"divop"
%token	T_POWEROP	"powerop"

// List Functions
%token	T_LISTFUNC	"listfunc"
	
// String
%token	<strval>	T_STRING	"string"
	
//Other
%token	T_LPAREN	"lparen"
%token	T_RPAREN	"rparen"
%token	T_COMMA		"comma"
%token	T_ASSIGN	"assign"
%token	T_DOT		"dot"
%token	T_COLON		"colon"
%token	T_LBRACK	"lbrack"
%token	T_RBRACK	"rbrack"

%token	T_EOF	0	"EOF"
 
%%

program:		body T_END subprograms
body:			declarations statements
declarations:	declarations type vars
				| declarations T_RECORD fields T_ENDREC vars
				| declarations T_DATA vals
				| %empty
type:			T_INTEGER | T_REAL | T_LOGICAL | T_CHARACTER | T_COMPLEX
vars:			vars T_COMMA undef_variable
				| undef_variable
undef_variable:	T_LIST undef_variable
				| T_ID T_LPAREN dims T_RPAREN
				| T_ID
dims:			dims T_COMMA dim
				| dim
dim:			T_ICONST | T_ID
fields:			fields field
				| field
field:			type vars
				| T_RECORD fields T_ENDREC vars
vals:			vals T_COMMA T_ID value_list
				| T_ID value_list
value_list:		T_DIVOP values T_DIVOP
values:			values T_COMMA value
				| value
value:			sign constant
				| T_STRING
sign:			T_ADDOP | %empty
constant:		simple_constant
				| complex_constant
simple_constant:	T_ICONST | T_RCONST | T_LCONST | T_CCONST
complex_constant:	T_LPAREN T_RCONST T_COLON sign T_RCONST T_RPAREN
statements:			statements labeled_statement
					| labeled_statement
labeled_statement:	label statement
					| statement
label:				T_ICONST
statement:			simple_statement
					| compound_statement
simple_statement:	assignment
					| goto_statement
					| if_statement
					| subroutine_call
					| io_statement
					| T_CONTINUE
					| T_RETURN
					| T_STOP
assignment:	variable T_ASSIGN expression
			| variable T_ASSIGN T_STRING
variable:	variable T_DOT T_ID
			| variable T_LPAREN expressions T_RPAREN
			| T_LISTFUNC T_LPAREN expression T_RPAREN
			| T_ID
expressions:	expressions T_COMMA expression
				| expression
expression:	expression T_OROP expression
			| expression T_ANDOP expression
			| expression T_RELOP expression
			| expression T_ADDOP expression
			| expression T_MULOP expression
			| expression T_DIVOP expression
			| expression T_POWEROP expression
			| T_NOTOP expression
			| T_ADDOP expression
			| variable
			| simple_constant
			| T_LENGTH T_LPAREN expression T_RPAREN
			| T_NEW T_LPAREN expression T_RPAREN
			| T_LPAREN expression T_RPAREN
			| T_LPAREN expression T_COLON expression T_RPAREN
			| listexpression
listexpression:	T_LBRACK expressions T_RBRACK
				| T_LBRACK T_RBRACK
goto_statement:	T_GOTO label
				| T_GOTO T_ID T_COMMA T_LPAREN labels T_RPAREN
labels:	labels T_COMMA label
		| label
if_statement:	T_IF T_LPAREN expression T_RPAREN label T_COMMA label T_COMMA label
				| T_IF T_LPAREN expression T_RPAREN simple_statement
subroutine_call:	T_CALL variable
io_statement:	T_READ read_list
				| T_WRITE write_list
read_list:	read_list T_COMMA read_item
			| read_item
read_item:	variable
			| T_LPAREN read_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN
iter_space:	expression T_COMMA expression step
step:	T_COMMA expression
		| %empty
write_list:	write_list T_COMMA write_item
			| write_item
write_item:	expression
			| T_LPAREN write_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN
			| T_STRING
compound_statement:	branch_statement
					| loop_statement
branch_statement:	T_IF T_LPAREN expression T_RPAREN T_THEN body tail
tail:	T_ELSE body T_ENDIF
		| T_ENDIF
loop_statement:	T_DO T_ID T_ASSIGN iter_space body T_ENDDO
subprograms:	subprograms subprogram
				| %empty
subprogram:	header body T_END
header:	type T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN
		| T_LIST T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN
		| T_SUBROUTINE T_ID T_LPAREN formal_parameters T_RPAREN
		| T_SUBROUTINE T_ID
formal_parameters:	type vars T_COMMA formal_parameters
					| type vars

%%

int main(int argc, char **argv)
{
	int token;

	if (argc > 1) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			perror("Error opening file");
			return -1;
		}
	}
	
	yyparse();

	fclose(yyin);
	return 0;
}
