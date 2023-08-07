%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <errno.h>
	#include <stdbool.h>
	#include "parser.tab.h"
	#include "hashtbl.h"

	extern FILE *yyin;
	extern int yylex();
	extern void yyerror(const char *err);

	HASHTBL *hashtbl;
	int scope = 0;
%}

%define parse.error verbose

%union	{
	int 		intval;
	bool 		lval;
	double		rval;
	char		charval;
	char *		strval;
}

//  optional required  optional  optional
// %token    <type>    <name>    <number>  "description"
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
%token	<strval>	T_ID	"id"

// Constants
%token	<intval>	T_ICONST	"iconst"
%token	<rval>		T_RCONST	"rconst"
%token	<lval>		T_LCONST	"lconst"
%token	<charval>	T_CCONST	"cconst"

// String
%token	<strval>	T_STRING	"string"

// Operators
%token	T_OROP		"orop"
%token	T_ANDOP		"andop"
%token	T_NOTOP		"notop"
%token	T_RELOP 	".GT. or .GE. or .LT. or .LE. or .EQ. or NE."
%token	T_ADDOP		"+ or -"
%token	T_MULOP		"mulop"
%token	T_DIVOP		"divop"
%token	T_POWEROP	"powerop"

// List Functions
%token	T_LISTFUNC	"listfunc"

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
 
// Declaring types for non-terminal variables
%type <strval> program body declarations type vars undef_variable dims dim fields field vals value_list values value sign constant simple_constant complex_constant statements labeled_statement label statement simple_statement assignment variable expressions expression listexpression goto_statement labels if_statement subroutine_call io_statement read_list read_item iter_space step write_list write_item compound_statement branch_statement tail loop_statement subprograms subprogram header formal_parameters



/* Declaring ascociativities and priorities */
/* Operators listed in descending priority
with specified associativity (left/right/nonassoc) */
%nonassoc error
%left T_OROP
%left T_ANDOP
%nonassoc T_NOTOP
%nonassoc T_RELOP
%left T_ADDOP
%left T_MULOP T_DIVOP
%right T_POWEROP

%%

program:			body T_END subprograms
					/* | body error T_EOF { yyerror("Expected keyword 'end' at the end of the program"); yyerrok; } */

body:				declarations statements

declarations:		declarations type vars
					| declarations T_RECORD fields T_ENDREC vars
					| declarations T_DATA vals
					| %empty

type:				T_INTEGER | T_REAL | T_LOGICAL | T_CHARACTER | T_COMPLEX

vars:				vars T_COMMA undef_variable
					| vars error undef_variable { yyerror("Missing seperator ',' between variable definitions"); yyerrok; }
					| undef_variable

undef_variable:		T_LIST undef_variable
					| T_ID T_LPAREN dims T_RPAREN	{hashtbl_insert(hashtbl, $1, NULL, scope);}
					| T_ID							{hashtbl_insert(hashtbl, $1, NULL, scope);}

dims:				dims T_COMMA dim
					| dim

dim:				T_ICONST 
					| T_ID							{hashtbl_insert(hashtbl, $1, NULL, scope);}

fields:				fields field
					| field

field:				type vars
					| T_RECORD fields T_ENDREC vars

vals:				vals T_COMMA T_ID value_list	{hashtbl_insert(hashtbl, $3, NULL, scope);}
					| T_ID value_list				{hashtbl_insert(hashtbl, $1, NULL, scope);}

value_list:			T_DIVOP values T_DIVOP

values:				values T_COMMA value
					| value

value:				sign constant
					| T_STRING

sign:				T_ADDOP | %empty

constant:			simple_constant
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

assignment:			variable T_ASSIGN expression
					| variable T_ASSIGN T_STRING

variable:			variable T_DOT T_ID					{hashtbl_insert(hashtbl, $3, NULL, scope);}
					| variable T_LPAREN expressions T_RPAREN
					| T_LISTFUNC T_LPAREN expression T_RPAREN
					| T_ID								{hashtbl_insert(hashtbl, $1, NULL, scope);}

expressions:		expressions T_COMMA expression
					| expression

expression:			expression T_OROP expression
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
					| expression error expression { yyerror("Expected operator or seperator ',' between expressions"); yyerrok; }

listexpression:		T_LBRACK expressions T_RBRACK
					| T_LBRACK T_RBRACK

goto_statement:		T_GOTO label
					| T_GOTO T_ID T_COMMA T_LPAREN labels T_RPAREN		{hashtbl_insert(hashtbl, $2, NULL, scope);}

labels:				labels T_COMMA label
					| labels error label { yyerror("Missing seperator between labels"); yyerrok; }
					| label

if_statement:		T_IF T_LPAREN expression T_RPAREN {scope++;} label T_COMMA label T_COMMA label {hashtbl_get(hashtbl, scope); scope--;}
					| T_IF T_LPAREN expression T_RPAREN {scope++;} simple_statement{hashtbl_get(hashtbl, scope); scope--;}

subroutine_call:	T_CALL variable

io_statement:		T_READ read_list
					| T_WRITE write_list

read_list:			read_list T_COMMA read_item
					| read_item

read_item:			variable
					| T_LPAREN read_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN	{hashtbl_insert(hashtbl, $4, NULL, scope);}

iter_space:			expression T_COMMA expression step

step:				T_COMMA expression
					| %empty

write_list:			write_list T_COMMA write_item
					| write_item

write_item:			expression
					| T_LPAREN write_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN		{hashtbl_insert(hashtbl, $4, NULL, scope);}
					| T_STRING

compound_statement:	branch_statement
					| loop_statement

branch_statement:	T_IF T_LPAREN expression T_RPAREN T_THEN {scope++;} body tail

tail:				T_ELSE {scope--; hashtbl_get(hashtbl, scope); scope++;}  body T_ENDIF {hashtbl_get(hashtbl, scope); scope--;}
					| T_ENDIF {hashtbl_get(hashtbl, scope); scope--;}

loop_statement:		T_DO T_ID T_ASSIGN iter_space {scope++;}body T_ENDDO	{hashtbl_insert(hashtbl, $2, NULL, scope);hashtbl_get(hashtbl, scope); scope--;}

subprograms:		subprograms subprogram
					| %empty

subprogram:			header body T_END // The subprograms have global scope, so we do not have to increase the scope variable

header:				type T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN	{hashtbl_insert(hashtbl, $3, NULL, scope);}
					| T_LIST T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN	{hashtbl_insert(hashtbl, $3, NULL, scope);}
					| T_SUBROUTINE T_ID T_LPAREN formal_parameters T_RPAREN			{hashtbl_insert(hashtbl, $2, NULL, scope);}
					| T_SUBROUTINE T_ID											{hashtbl_insert(hashtbl, $2, NULL, scope);}

formal_parameters:	type vars T_COMMA formal_parameters
					| type vars

%%

int main(int argc, char **argv)
{
	if(!(hashtbl = hashtbl_create(10, NULL))) {
        fprintf(stderr, "ERROR: hashtbl_create() failed!\n");
        exit(EXIT_FAILURE);
    }

	if (argc > 1) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			perror("Error opening file");
			return -1;
		}
	}
	
	yyparse();

	hashtbl_get(hashtbl, scope); // Retrieve the last table (Scope 0);
    hashtbl_destroy(hashtbl);

	fclose(yyin);
	return 0;
}
