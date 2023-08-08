/*------------ DECLARATION SECTION ------------*/
%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <errno.h>
	#include <math.h>
	#include <stdbool.h>
	#include "parser.tab.h"
	#include "constants.h"
	
	#define GET_INT_VALUE(text, base) (strtol(text + 2, NULL, base))
	#define GET_REAL_VALUE(text, base) (base == 16 ? hextod(text): bintod(text))
	#define IS_ID(token) (token==T_ID)
	#define IS_KEYWORD(token) (in(token, keywords, 27))
	#define IS_OPERATOR(token) (in(token, operators, 17))
	#define IS_CCONST(token) (token==T_CCONST)
	#define IS_ICONST(token) (token==T_ICONST)
	#define IS_RCONST(token) (token==T_RCONST)
	#define IS_LCONST(token) (token==T_LCONST)
	#define IS_STRING(token) (token==T_STRING)
	#define IS_LISTFUNC(token) (token==T_LISTFUNC)
	#define IS_EOF(token) (token==T_EOF)
	
	char string_buffer[2*MAX_STRING_LENGTH] = {0};
	const int string_buffer_size = 2*MAX_STRING_LENGTH;
	int string_buffer_length = 0;
	char c;
	char *ptr;

	int keywords[] = {T_FUNCTION, T_SUBROUTINE, T_END, T_INTEGER, T_REAL,
					  T_LOGICAL, T_CHARACTER, T_COMPLEX, T_RECORD, T_ENDREC,
					  T_LIST, T_DATA, T_CONTINUE, T_GOTO, T_CALL, T_READ, T_WRITE,
					  T_NEW, T_LENGTH, T_IF, T_THEN, T_ELSE, T_ENDIF, T_DO,
					  T_ENDDO, T_STOP, T_RETURN};

	int operators[] = {T_ADDOP, T_ANDOP, T_OROP, T_NOTOP, T_RELOP, T_ADDOP,	
					   T_MULOP, T_DIVOP, T_POWEROP, T_LPAREN, T_RPAREN,
					   T_COMMA, T_ASSIGN, T_DOT, T_COLON, T_LBRACK, T_RBRACK};

	char *end_string_wrapper();
	bool read_string_part();
	char *get_character(char *string, char *cdest);
	double hextod(char *str);
	double bintod(char *str);
	void yyerror(const char *s);
	bool in(int value, int *array, int size);
	int print_token(int token);
%}

%option case-insensitive
%option yylineno
%option noyywrap

%x COMMENT CHARACTER STRING

WHITECHAR	[ \t\v\n]

ALPHA		[a-z]
NUM			[0-9]
ALNUM		[0-9a-z]
ID			(_{ALPHA}(_?{ALNUM})*_)|({ALPHA}{ALNUM}*)
WRONG_ID_UNDERSCORE	(({ALNUM}+_)+)|((_{ALNUM}+)+)|({ALNUM}+(_{ALNUM}*)+)
WRONG_ID_NONALPHA	(_{NUM}(_?{ALNUM})*_)|({NUM}{ALNUM}*)


VALID_DNUM	([1-9][0-9]*)|0
VALID_HNUM	([1-9A-F][0-9A-F]*)
VALID_BNUM	(1[01]*)

/* Integer Constants */
ICONST_D	{VALID_DNUM}
ICONST_H	0H{VALID_HNUM}
ICONST_B	0B{VALID_BNUM}

/* Decimal Real Constants */
RCONST_D_1	{VALID_DNUM}?\.([0-9]*[1-9][0-9]*|0)
RCONST_D_2	{VALID_DNUM}(\.([0-9]*[1-9][0-9]*|0)?)?
RCONST_D	({RCONST_D_1}|{RCONST_D_2})(E[+-]?{VALID_DNUM})?

/* Hexadecimal Real Constants */
RCONST_H_1	({VALID_HNUM}|0)?\.([0-9A-F]*[1-9A-F][0-9A-F]*|0)
RCONST_H_2	{VALID_HNUM}(\.(([0-9A-F]*[1-9A-F][0-9A-F]*)|0)?)?
RCONST_H	0H({RCONST_H_1}|{RCONST_H_2})

/* Binary Real Constants */
RCONST_B_1	({VALID_BNUM}|0)?\.([01]*1[01]*|0)
RCONST_B_2	{VALID_BNUM}(\.(([01]*1[01]*)|0)?)?
RCONST_B	0B({RCONST_B_1}|{RCONST_B_2})


%%
 /*------------ RULES SECTION ---------------*/

 
<COMMENT>{
\n		{ BEGIN(INITIAL); }
[^\n]+
}

<CHARACTER>{
""([" "-~]|\\[nftrbv])"'"	{ ptr = get_character(yytext, &c); yylval.charval = c; BEGIN(INITIAL); return print_token(T_CCONST); }
"'"							{ yyerror("Empty character constant"); BEGIN(INITIAL); }
<<EOF>>						{ yyerror("End of file inside character constant"); }
.*"'"						{ yyerror("Unexpected content of character constant"); BEGIN(INITIAL); }
}

<STRING>{
([^"\\\n]|(\\[\"\\nftrbv]))+/\\\n	{ read_string_part(); }
\\\n								{}
([^"\\\n]|(\\[\"\\nftrbv]))+/\"		{ read_string_part(); }
\"									{ yylval.strval = end_string_wrapper(); BEGIN(INITIAL); return print_token(T_STRING); }
<<EOF>>								{ yyerror("End of file inside character constant"); }
.									{ yyerror("Unexpected character inside string"); }
}

<INITIAL>{
"$"				{ BEGIN(COMMENT); }

<<EOF>>			{ return print_token(T_EOF); }
{WHITECHAR}		{}

 /* Keywords */
"FUNCTION"		{ return print_token(T_FUNCTION	); }
"SUBROUTINE"	{ return print_token(T_SUBROUTINE); }
"END"			{ return print_token(T_END); }
"INTEGER"		{ return print_token(T_INTEGER); }
"REAL"			{ return print_token(T_REAL); }
"LOGICAL"		{ return print_token(T_LOGICAL); }
"CHARACTER"		{ return print_token(T_CHARACTER); }
"COMPLEX"		{ return print_token(T_COMPLEX); }
"RECORD"		{ return print_token(T_RECORD); }
"ENDREC"		{ return print_token(T_ENDREC); }
"LIST"			{ return print_token(T_LIST); }
"DATA"			{ return print_token(T_DATA); }
"CONTINUE"		{ return print_token(T_CONTINUE); }
"GOTO"			{ return print_token(T_GOTO); }
"CALL"			{ return print_token(T_CALL); }
"READ"			{ return print_token(T_READ); }
"WRITE"			{ return print_token(T_WRITE); }
"NEW"			{ return print_token(T_NEW); }
"LENGTH"		{ return print_token(T_LENGTH); }
"IF"			{ return print_token(T_IF); }
"THEN"			{ return print_token(T_THEN); }
"ELSE"			{ return print_token(T_ELSE); }
"ENDIF"			{ return print_token(T_ENDIF); }
"DO"			{ return print_token(T_DO); }
"ENDDO"			{ return print_token(T_ENDDO); }
"STOP"			{ return print_token(T_STOP); }
"RETURN"		{ return print_token(T_RETURN); }

 /* Iconst */
{ICONST_D}		{ yylval.intval = atoi(yytext); return print_token(T_ICONST); }
{ICONST_H}		{ yylval.intval = GET_INT_VALUE(yytext, 16); return print_token(T_ICONST); }
{ICONST_B}		{ yylval.intval = GET_INT_VALUE(yytext, 2); return print_token(T_ICONST); }

 /* Rconst */
{RCONST_D}		{ yylval.intval = strtod(yytext, NULL); return print_token(T_RCONST); }
{RCONST_H}		{ yylval.intval = GET_REAL_VALUE(yytext, 16); return print_token(T_RCONST); }
{RCONST_B}		{ yylval.intval = GET_REAL_VALUE(yytext, 2); return print_token(T_RCONST); }

 /* Lconst */
".TRUE."		{ yylval.lval = true;  return print_token(T_LCONST); }
".FALSE."		{ yylval.lval = false; return print_token(T_LCONST); }


 /* Operators */
".AND."					{ return print_token(T_ANDOP); }
".OR."					{ return print_token(T_OROP); }
".NOT."					{ return print_token(T_NOTOP); }
"."([GL][TE]|EQ|NE)"."	{ return print_token(T_RELOP); }
"+"|"-"					{ return print_token(T_ADDOP); }
"*"						{ return print_token(T_MULOP); }
"/"						{ return print_token(T_DIVOP); }
"**"					{ return print_token(T_POWEROP); }


 /*Extra lexical units*/
"("				{ return print_token(T_LPAREN); }
")"				{ return print_token(T_RPAREN); }
","				{ return print_token(T_COMMA); }
"="				{ return print_token(T_ASSIGN); }
"."				{ return print_token(T_DOT); }
":"				{ return print_token(T_COLON); }
"["				{ return print_token(T_LBRACK); }
"]"				{ return print_token(T_RBRACK); }

 /* Characters */
"'"				{ BEGIN(CHARACTER); }

\"				{ BEGIN(STRING); }

 /* List Functions */
"C"(AD*|D+)"R"	{ return print_token(T_LISTFUNC); }

 /* Identifier */
{ID}			{ yylval.strval = strdup(yytext); return print_token(T_ID); }
{WRONG_ID_UNDERSCORE}	{ yyerror("Ids that contain '_' have to start and end in '_'"); }
{WRONG_ID_NONALPHA}		{ yyerror("Ids' first character (after the optional '_') must be an alphabetical character"); }

.				{ yyerror("Unknown Character");}
}
%%
/*------------ USER-CODE SECTION ------------*/

/* Reads string_buffer and creates final string, also clears string_buffer and string_buffer_length */
char *end_string_wrapper() {
	int next_idx;
	char *new_str, *ptr;
	char c;

	if (!(new_str = (char *)malloc(MAX_STRING_LENGTH * sizeof(char)))) {
		perror("Error");
		exit(EXIT_FAILURE);
	}
	
	next_idx = 0;
	ptr = string_buffer;
	do {
		ptr = get_character(ptr, &c);
		new_str[next_idx++] = c;
	} while(ptr != NULL);
	
	if (!(new_str = (char *)realloc(new_str, next_idx * sizeof(char)))) {
		perror("Error");
		exit(EXIT_FAILURE);
	}

	// Reset buffer
	string_buffer[0] = '\0';
	string_buffer_length = 0;

	return new_str;
}

/* Appends the part of the string to string_buffer, returns true if success */
bool read_string_part(void)
{
	if (strncat(string_buffer, yytext, string_buffer_size - string_buffer_length) != string_buffer) {
		return false;
	}
	else {
		string_buffer_length += strlen(yytext);
		return true;
	}
}

/* next = get_character(string, &c); */
char *get_character(char *string, char *cdest)
{
	if (string[0] == '\0') {
		*cdest = '\0';
		return NULL;
	}
	else if (string[0] != '\\') {
		*cdest = string[0];
		return string + 1;
	}
	else {
		switch (string[1]) {
			case 'n' : {
				*cdest = '\n';
				break;
			}
			case 'f' : {
				*cdest = '\f';
				break;
			}
			case 't' : {
				*cdest = '\t';
				break;
			}
			case 'r' : {
				*cdest = '\r';
				break;
			}
			case 'b' : {
				*cdest = '\b';
				break;
			}
			case 'v' : {
				*cdest = '\v';
				break;
			}
			case '\\' : {
				*cdest = '\\';
				break;
			}
			case '\"' : {
				*cdest = '\"';
				break;
			}
		}
		return string + 2;
	}
}

/* Parse a binary real number */
double bintod(char *str)
{
	long long int a, b;
	char *ptr;

	//Extract the whole number part
	ptr = strtok(str, ".");
	a = strtoll(ptr, NULL, 2);

	//Extract the fractional part
	ptr = strtok(NULL, ".");
	b = strtoll(ptr, NULL, 2);

	//Calculate the decimal number
	return (a + ((double) b/pow(2,strlen(ptr))));
}

/* Parse a hexadecimal real number */
double hextod(char *str)
{
	long long int a, b;
	char *ptr;
	char delim[] = ".";

	//Extract the whole number part
	ptr = strtok(str, delim);
	a = strtoll(ptr, NULL, 16);

	//Extract the fractional part
	ptr = strtok(NULL, delim);
	b = strtoll(ptr, NULL, 16);

	//Calculate the decimal number
	return (a + ((double) b/pow(16,strlen(ptr))));
}

void yyerror(const char *s)
{
	static int errors = 0;
	errors++;

	printf("ERROR: %s in line %d\n", s, yylineno);

	if (errors > 5) {
		printf("%d errors encountered. Compilation not possible...\n", errors);
		exit(1);
	}
}

/* Checks if `value` exists in `array` with `size` elements */
bool in(int value, int *array, int size) {
	for (int i = 0; i < size; i++) {
		if (value == array[i]);
			return true;
	}
	return false;
}

/* Prints a corresponding message for each token */
/* All messages are printed by defining VERBOSE_LEXER in constants.h */
int print_token(int token) {
#ifdef VERBOSE_LEXER 
	if (IS_ID(token)) {
		printf("Found id \"%s\", in line %d\n", yytext, yylineno);
	}
	else if (IS_KEYWORD(token)) {
		printf("Found keyword \"%s\", in line %d\n", yytext, yylineno);
	}
	else if (IS_OPERATOR(token)) {
		printf("Found operator \"%s\", in line %d\n", yytext, yylineno);
	}
	else if (IS_CCONST(token)) {
		printf("Found character '%c' in line %d\n", yylval.charval, yylineno);
	}
	else if (IS_ICONST(token)) {
		printf("Found integer %d  as \"%s\", in line %d\n", yylval.intval, yytext, yylineno);
	}
	else if (IS_RCONST(token)) {
		printf("Found real %lf as \"%s\", in line %d\n", yylval.rval, yytext, yylineno);
	}
	else if (IS_LCONST(token)) {
		printf("Found bool %s, in line %d\n", (yylval.lval==1 ? "True" : "False"), yylineno);
	}
	else if (IS_STRING(token)) {
		printf("Found string \"%s\", in line %d\n", yylval.strval, yylineno);
	}
	else if (IS_LISTFUNC(token)) {
		printf("Found list function \"%s\", in line %d\n", yytext, yylineno);
	}
	else if (IS_EOF(token)) {
		printf("Found EOF\n");
	}
	else {
		yyerror("Unknown token\n");
		exit(1);
	}
#endif

	return token;
}