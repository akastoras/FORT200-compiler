%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <errno.h>
	#include <stdbool.h>
	#include "lib/ast.h"
	#include "lib/symbol_table.h"
	
	#include "parser.tab.h"

	extern FILE *yyin;
	extern int yylex();
	extern void yyerror(const char *err);
%}

%define parse.error verbose

%union	{
	int 			intval;
	bool 			lval;
	double			rval;
	char			charval;
	char			*strval;

	AST_Sign		signval;
	AST_Type		typeval;
	AST_Constant	*constval;
	AST_Values		*values;
	// AST_Expression	*exprval;
}

//  optional required  optional  optional
// %token    <type>    <name>    <number>  "description"
%token T_FUNCTION	"function"
%token T_SUBROUTINE	"subroutine"
%token T_END		"end"
%token T_INTEGER	"int"
%token T_REAL		"real"
%token T_LOGICAL 	"logical"
%token T_CHARACTER 	"char"
%token T_COMPLEX 	"complex"
%token T_RECORD		"record"
%token T_ENDREC		"endrec"
%token T_LIST		"list"
%token T_DATA		"data"
%token T_CONTINUE	"continue"
%token T_GOTO		"goto"
%token T_CALL		"call"
%token T_READ		"read"
%token T_WRITE		"write"
%token T_NEW		"new"
%token T_LENGTH		"length"
%token T_IF			"if" 
%token T_THEN		"then" 
%token T_ELSE		"else"
%token T_ENDIF		"endif"
%token T_DO			"do"
%token T_ENDDO		"enddo"
%token T_STOP		"stop"
%token T_RETURN		"return"
	
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
%token	<signval>	T_ADDOP		"+ or -"
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
%type <strval> program body declarations vars undef_variable dims dim fields field vals statements labeled_statement label statement simple_statement assignment variable expressions listexpression goto_statement labels if_statement subroutine_call io_statement read_list read_item iter_space step write_list write_item compound_statement branch_statement tail loop_statement subprograms subprogram header formal_parameters
%type <typeval> type
%type <constval> constant simple_constant complex_constant value
%type <signval> sign
%type <values> values value_list
/* %type <exprval> expression */


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

type:				T_INTEGER		{ $$ = INT; }
					| T_REAL		{ $$ = REAL; }
					| T_LOGICAL 	{ $$ = LOG; }
					| T_CHARACTER	{ $$ = CHAR; }
					| T_COMPLEX		{ $$ = CMPLX; } 

vars:				vars T_COMMA undef_variable
					| vars error undef_variable { yyerror("Missing seperator ',' between variable definitions"); yyerrok; }
					| undef_variable

undef_variable:		T_LIST undef_variable
					| T_ID T_LPAREN dims T_RPAREN	{ hashtbl_insert(symbol_table, $1, create_entry(), scope); }
					| T_ID							{ hashtbl_insert(symbol_table, $1, NULL, scope); }

dims:				dims T_COMMA dim
					| dim

dim:				T_ICONST
					| T_ID

fields:				fields field
					| field

field:				type vars
					| T_RECORD fields T_ENDREC vars

// Value declerations, typechecking must be done
vals:				vals T_COMMA T_ID value_list { $$ = ast_insert_val_to_vals($1, $3, $4); }
					| T_ID value_list { $$ = ast_insert_val_to_vals(NULL, $1, $2); }

// Propagate the values to value_list
value_list:			T_DIVOP values T_DIVOP { $$ = $2; ast_print_values($$); }

// Form an array for values
values:				values T_COMMA value { $$ = ast_insert_value_to_values($1, $3); }
					| value { $$ = ast_insert_value_to_values(NULL, $1); }

// Get value from sign and constant make sure that the constant type takes a sign
value:				sign constant { $$ = ast_get_value($1, $2); }
					| T_STRING { $$ = ast_get_string($1); }

// Get sign as NONE, PLUS, or MINUS
sign:				T_ADDOP { $$ = ast_get_sign($1); }
					| %empty { $$ = ast_get_sign(NONE); }

// In constant just propagate the value of simple or complex constant
constant:			simple_constant { $$ = $1; }
					| complex_constant { $$ = $1; }

// Give a type depending on the token returned from lexer, and copy the value
simple_constant:	T_ICONST { $$ = ast_get_ICONST($1); }
					| T_RCONST { $$ = ast_get_RCONST($1); }
					| T_LCONST { $$ = ast_get_LCONST($1); }
					| T_CCONST { $$ = ast_get_CCONST($1); }

// Give the complex type as well as the real and imaginary values of the complex number
complex_constant:	T_LPAREN T_RCONST T_COLON sign T_RCONST T_RPAREN { $$ = ast_get_CMPLX($2, $4, $5); }

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
					| T_GOTO T_ID T_COMMA T_LPAREN labels T_RPAREN	{ hashtbl_insert(symbol_table, $2, NULL, scope); }

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

branch_statement:	T_IF T_LPAREN expression T_RPAREN T_THEN { stbl_increase_scope(); } body tail

tail:				T_ELSE { hashtbl_get(symbol_table, scope); }  body T_ENDIF { hashtbl_get(symbol_table, scope); stbl_decrease_scope(); }
					| T_ENDIF { hashtbl_get(symbol_table, scope); stbl_decrease_scope(); }

loop_statement:		T_DO T_ID T_ASSIGN iter_space {stbl_increase_scope();} body T_ENDDO { hashtbl_insert(symbol_table, $2, NULL, scope); hashtbl_get(symbol_table, scope); stbl_decrease_scope(); }

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
