%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <errno.h>
	#include <stdbool.h>
	#include "ast.h"
	#include "hashtbl.h"
	
	#include "parser.tab.h"

	extern FILE *yyin;
	extern int yylex();
	extern void yyerror(const char *err);

	HASHTBL *symbol_table;
	int scope = 0;
%}

%define parse.error verbose

%union	{
	int 			intval;
	bool 			lval;
	double			rval;
	char			charval;
	char			*strval;

	AST_Type		*typeval;
	AST_Constant	*constval;
	AST_Sign		*signval;
	// AST_Expression	*exprval;
}

//  optional required  optional  optional
// %token    <type>    <name>    <number>  "description"
%token	<strval> T_FUNCTION		"function"
%token	<strval> T_SUBROUTINE	"subroutine"
%token	<strval> T_END			"end"
%token	<strval> T_INTEGER		"int"
%token	<strval> T_REAL			"real"
%token	<strval> T_LOGICAL 		"logical"
%token	<strval> T_CHARACTER 	"char"
%token	<strval> T_COMPLEX 		"complex"
%token	<strval> T_RECORD		"record"
%token	<strval> T_ENDREC		"endrec"
%token	<strval> T_LIST			"list"
%token	<strval> T_DATA			"data"
%token	<strval> T_CONTINUE		"continue"
%token	<strval> T_GOTO			"goto"
%token	<strval> T_CALL			"call"
%token	<strval> T_READ			"read"
%token	<strval> T_WRITE		"write"
%token	<strval> T_NEW			"new"
%token	<strval> T_LENGTH		"length"
%token	<strval> T_IF			"if" 
%token	<strval> T_THEN			"then" 
%token	<strval> T_ELSE			"else"
%token	<strval> T_ENDIF		"endif"
%token	<strval> T_DO			"do"
%token	<strval> T_ENDDO		"enddo"
%token	<strval> T_STOP			"stop"
%token	<strval> T_RETURN		"return"
	
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
%token	<strval>	T_OROP		"orop"
%token	<strval>	T_ANDOP		"andop"
%token	<strval>	T_NOTOP		"notop"
%token	<strval>	T_RELOP 	".GT. or .GE. or .LT. or .LE. or .EQ. or NE."
%token	<strval>	T_ADDOP		"+ or -"
%token	<strval>	T_MULOP		"mulop"
%token	<strval>	T_DIVOP		"divop"
%token	<strval>	T_POWEROP	"powerop"

// List Functions
%token	T_LISTFUNC	"listfunc"

//Other
%token	<strval>	T_LPAREN	"lparen"
%token	<strval>	T_RPAREN	"rparen"
%token	<strval>	T_COMMA		"comma"
%token	<strval>	T_ASSIGN	"assign"
%token	<strval>	T_DOT		"dot"
%token	<strval>	T_COLON		"colon"
%token	<strval>	T_LBRACK	"lbrack"
%token	<strval>	T_RBRACK	"rbrack"

%token	T_EOF	0	"EOF"
 
// Declaring types for non-terminal variables
%type <strval> program body declarations vars undef_variable dims dim fields field vals value_list values value statements labeled_statement label statement simple_statement assignment variable expressions listexpression goto_statement labels if_statement subroutine_call io_statement read_list read_item iter_space step write_list write_item compound_statement branch_statement tail loop_statement subprograms subprogram header formal_parameters
%type <typeval> type
%type <constval> constant simple_constant complex_constant
%type <signval> sign
%type <exprval> expression


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

program:			body T_END subprograms { hashtbl_get(symbol_table, scope); }
					/* | body error T_EOF { yyerror("Expected keyword 'end' at the end of the program"); yyerrok; } */

body:				declarations statements

declarations:		declarations type vars
					| declarations T_RECORD fields T_ENDREC vars
					| declarations T_DATA vals
					| %empty

type:				T_INTEGER		{ $$ = ast_get_type_int(); }
					| T_REAL		{ $$ = ast_get_type_real(); }
					| T_LOGICAL 	{ $$ = ast_get_type_logical(); }
					| T_CHARACTER	{ $$ = ast_get_type_character(); }
					| T_COMPLEX		{ $$ = ast_get_type_complex(); } 

vars:				vars T_COMMA undef_variable
					| vars error undef_variable { yyerror("Missing seperator ',' between variable definitions"); yyerrok; }
					| undef_variable

undef_variable:		T_LIST undef_variable
					| T_ID T_LPAREN dims T_RPAREN	{ hashtbl_insert(symbol_table, $1, NULL, scope); }
					| T_ID							{ hashtbl_insert(symbol_table, $1, NULL, scope); }

dims:				dims T_COMMA dim
					| dim

dim:				T_ICONST 
					| T_ID							{ hashtbl_insert(symbol_table, $1, NULL, scope); }

fields:				fields field
					| field

field:				type vars
					| T_RECORD fields T_ENDREC vars

vals:				vals T_COMMA T_ID value_list	{ hashtbl_insert(symbol_table, $3, NULL, scope); }
					| T_ID value_list				{ hashtbl_insert(symbol_table, $1, NULL, scope); }

value_list:			T_DIVOP values T_DIVOP

values:				values T_COMMA value
					| value

// Get value from sign and constant
// make sure that the constant type takes a sign
value:				sign constant
					| T_STRING

// Get sign as NONE, POSITIVE, or NEGATIVE
sign:				T_ADDOP { $$ = ast_get_sign($1); }
					| %empty { $$ = ast_get_sign(NULL); }

// In constant just propagate the value of simple or complex constant
constant:			simple_constant { $$ = $1; }
					| complex_constant { $$ = $1; }

// Give a type depending on the token returned from lexer, and copy the value
simple_constant:	T_ICONST { $$ = ast_get_ICONST($1); }
					| T_RCONST { $$ = ast_get_RCONST($1); }
					| T_LCONST { $$ = ast_get_LCONST($1); }
					| T_CCONST { $$ = ast_get_CCONST($1); }

// Give the complex type as well as the real and imaginary values of the complex number
complex_constant:	T_LPAREN T_RCONST T_COLON sign T_RCONST T_RPAREN { $$ = ast_get_CMPLX($2, $5); }
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

variable:			variable T_DOT T_ID
					| variable T_LPAREN expressions T_RPAREN
					| T_LISTFUNC T_LPAREN expression T_RPAREN
					| T_ID

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
					| T_GOTO T_ID T_COMMA T_LPAREN labels T_RPAREN		{ hashtbl_insert(symbol_table, $2, NULL, scope); }

labels:				labels T_COMMA label
					| labels error label { yyerror("Missing seperator between labels"); yyerrok; }
					| label

if_statement:		T_IF T_LPAREN expression T_RPAREN label T_COMMA label T_COMMA label
					| T_IF T_LPAREN expression T_RPAREN simple_statement

subroutine_call:	T_CALL variable

io_statement:		T_READ read_list
					| T_WRITE write_list

read_list:			read_list T_COMMA read_item
					| read_item

read_item:			variable
					| T_LPAREN read_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN	{ hashtbl_insert(symbol_table, $4, NULL, scope); }

iter_space:			expression T_COMMA expression step

step:				T_COMMA expression
					| %empty

write_list:			write_list T_COMMA write_item
					| write_item

write_item:			expression
					| T_LPAREN write_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN		{ hashtbl_insert(symbol_table, $4, NULL, scope); }
					| T_STRING

compound_statement:	branch_statement
					| loop_statement

branch_statement:	T_IF T_LPAREN expression T_RPAREN T_THEN {scope++;} body tail

tail:				T_ELSE { hashtbl_get(symbol_table, scope); }  body T_ENDIF { hashtbl_get(symbol_table, scope); scope--; }
					| T_ENDIF {hashtbl_get(symbol_table, scope); scope--;}
 
loop_statement:		T_DO T_ID T_ASSIGN iter_space {scope++;} body T_ENDDO { hashtbl_insert(symbol_table, $2, NULL, scope); hashtbl_get(symbol_table, scope); scope--; }

subprograms:		subprograms subprogram
					| %empty

subprogram:			header body T_END // The subprograms have global scope, so we do not have to increase the scope variable

header:				type T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN		{ hashtbl_insert(symbol_table, $3, NULL, scope); }
					| T_LIST T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN	{ hashtbl_insert(symbol_table, $3, NULL, scope); }
					| T_SUBROUTINE T_ID T_LPAREN formal_parameters T_RPAREN			{ hashtbl_insert(symbol_table, $2, NULL, scope); }
					| T_SUBROUTINE T_ID												{ hashtbl_insert(symbol_table, $2, NULL, scope); }

formal_parameters:	type vars T_COMMA formal_parameters
					| type vars

%%

int main(int argc, char **argv)
{
	// Create a hash table used as the symbol table
	if(!(symbol_table = hashtbl_create(10, NULL))) {
		fprintf(stderr, "ERROR: hashtbl_create() failed!\n");
		exit(EXIT_FAILURE);
	}

	// Get input file.
	if (argc > 1) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			char buff[20];
			sprintf(buff, "Error opening file %s", argv[1]);
			perror(buff);
			return -1;
		}
	}
	
	// This is where the magic happens
	yyparse();

	// Free memory & Cleanup
	hashtbl_destroy(symbol_table);
	fclose(yyin);

	return 0;
}
