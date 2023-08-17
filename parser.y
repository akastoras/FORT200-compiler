%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <errno.h>
	#include <stdbool.h>
	#include "lib/ast.h"
	#include "lib/semantic.h"
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
	AST_Vals		*vals;
	AST_Dims		*dims;
	AST_UndefVar	*undef_var;
	AST_Vars		*vars;
	AST_Field		*field;
	AST_Fields		*fields;
	AST_Decls		*declared_variables;
	
	AST_Statements	*statements;
	AST_Body		*body;
	AST_Header		*header;
	AST_Subprogram	*subprogram;
	AST_Subprograms *subprograms;
	AST_Program		*program;
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
%type <strval> labeled_statement label statement simple_statement assignment variable expressions listexpression goto_statement labels if_statement subroutine_call io_statement read_list read_item iter_space step write_list write_item compound_statement branch_statement tail loop_statement
%type <typeval> type
%type <constval> constant simple_constant complex_constant value
%type <signval> sign
%type <values> values value_list
%type <vals> vals
%type <intval> dim
%type <dims> dims
%type <undef_var> undef_variable
%type <vars> vars
%type <field> field
%type <fields> fields
%type <declared_variables> declarations

%type <statements> statements;
%type <body> body;
%type <header> header;
%type <vars> formal_parameters
%type <subprogram> subprogram;
%type <subprograms> subprograms;
%type <program> program
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

// High-level structure of a FORT200 program
program:			{ stbl_increase_scope(); } body T_END { stbl_clear_scope(); stbl_decrease_scope(); } subprograms { $$ = ast_get_program($2, $5); }
					/* | body error T_EOF { yyerror("Expected keyword 'end' at the end of the program"); yyerrok; } */

// Body consisting of variable declarations and statements
body:				declarations statements { ast_print_decls($1); ast_create_body($1, $2); }

// High-level structure for variable declarations
declarations:		declarations type vars { $$ = ast_insert_decl_to_decls($1, $2, NULL, $3); }
					| declarations T_RECORD fields T_ENDREC vars { $$ = ast_insert_decl_to_decls($1, REC, $3, $5); }
					| declarations T_DATA { SEM_check_initialization_position(); } vals { ast_insert_init_in_decls($4); }
					| %empty { $$ = NULL; }

// Get type from integer constant
type:				T_INTEGER		{ $$ = INT; }
					| T_REAL		{ $$ = REAL; }
					| T_LOGICAL 	{ $$ = LOG; }
					| T_CHARACTER	{ $$ = CHAR; }
					| T_COMPLEX		{ $$ = CMPLX; }

// Form an array of vars
vars:				vars T_COMMA undef_variable { $$ = ast_insert_var_to_vars($$, $3); }
					| vars error undef_variable { yyerror("Missing seperator ',' between variable definitions"); yyerrok; }
					| undef_variable 			{ $$ = ast_insert_var_to_vars(NULL, $1); }

// Form an undefined variable. Can be a scalar, an array or a list
undef_variable:		T_LIST undef_variable			{ $$ = ast_get_undef_var(LIST, NULL, NULL, $2); }
					| T_ID T_LPAREN dims T_RPAREN	{ $$ = ast_get_undef_var(ARRAY, $1, $3, NULL); }
					| T_ID							{ $$ = ast_get_undef_var(SCALAR, $1, NULL, NULL); }

// Form an array of dims
dims:				dims T_COMMA dim { $$ = ast_insert_dim_to_dims($1, $3); }
					| dim { $$ = ast_insert_dim_to_dims(NULL, $1); }

// Propagate the value to dim. Either directly (ICONST) or from the the symbol table (ID)
dim:				T_ICONST { $$ = $1; }
					| T_ID { $$ = stbl_get_int_initVal($1); }

// Form an array of fields
fields:				fields field { $$ = ast_insert_field_to_fields($1, $2); }
					| field { $$ = ast_insert_field_to_fields(NULL, $1); }

// Propagate the variables and their type. If field is a record propagate the array of its subfields as well
field:				type vars { $$ = ast_get_field($1, $2, NULL); }
					| T_RECORD fields T_ENDREC vars { $$ = ast_get_field(REC, $4, $2); }

// Value declerations, typechecking must be done
vals:				vals T_COMMA T_ID value_list { $$ = ast_insert_val_to_vals($1, $3, $4); }
					| T_ID value_list { $$ = ast_insert_val_to_vals(NULL, $1, $2); }

// Propagate the values to value_list
value_list:			T_DIVOP values T_DIVOP { $$ = $2; /*ast_print_values($$);*/ }
 
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

statements:			statements labeled_statement { $$ = NULL; }
					| labeled_statement { $$ = NULL; }

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
					| T_GOTO T_ID T_COMMA T_LPAREN labels T_RPAREN

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
					| T_LPAREN read_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN

iter_space:			expression T_COMMA expression step

step:				T_COMMA expression
					| %empty

write_list:			write_list T_COMMA write_item
					| write_item

write_item:			expression
					| T_LPAREN write_list T_COMMA T_ID T_ASSIGN iter_space T_RPAREN
					| T_STRING

compound_statement:	branch_statement
					| loop_statement

branch_statement:	T_IF T_LPAREN expression T_RPAREN T_THEN { stbl_increase_scope(); } body tail

tail:				T_ELSE { stbl_clear_scope(); }  body T_ENDIF { stbl_clear_scope(); stbl_decrease_scope(); }
					| T_ENDIF { stbl_clear_scope(); stbl_decrease_scope(); }

loop_statement:		T_DO T_ID T_ASSIGN iter_space { stbl_increase_scope(); } body T_ENDDO { stbl_clear_scope(); stbl_decrease_scope(); }

subprograms:		subprograms subprogram { $$ = ast_insert_subprogram_to_subprograms($1, $2); }
					| %empty { $$ = NULL; }

// The subprograms have global scope, so we do not have to increase the scope variable
subprogram:			header { stbl_increase_scope(); } body { stbl_clear_scope(); stbl_decrease_scope(); } T_END { $$ = ast_get_subprogram($1, $3); }

// AST_Header
header:				type T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN { $$ = ast_get_header(FUNCTION, $1, false, $3, $5); }
					| T_LIST T_FUNCTION T_ID T_LPAREN formal_parameters T_RPAREN { $$ = ast_get_header(FUNCTION, 0, true, $3, $5); }
					| T_SUBROUTINE T_ID T_LPAREN formal_parameters T_RPAREN { $$ = ast_get_header(SUBPROGRAM, 0, false, $2, $4); }
					| T_SUBROUTINE T_ID { $$ = ast_get_header(SUBPROGRAM, 0, false, $2, NULL); }

formal_parameters:	type vars T_COMMA formal_parameters
					| type vars

%%

int main(int argc, char **argv)
{
	// Create a hash table used as the symbol table
	stbl_create();

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
	stbl_destroy();
	fclose(yyin);

	return 0;
}
