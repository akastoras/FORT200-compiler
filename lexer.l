/*------------ DECLARATION SECTION ------------*/
%{
	#include "lexer.h"
	#include <stdio.h>
	#include <stdlib.h>
	#include <unistd.h>
	#include <string.h>
	#include <math.h>

	#define GET_INT_VALUE(text, base) (strtol(text + 2, NULL, base))
	#define GET_REAL_VALUE(text, base) (base == 16 ? hextod(text): bintod(text))
	
	double hextod(char *str);
	double bintod(char *str);
%}

%option case-insensitive
%option yylineno
%option noyywrap

%x COMMENT

WHITECHAR	[ \t\v\n]

DIGIT		[0-9]
ALPHA		[a-z]
ALNUM		[0-9a-z]
ID			(_{ALPHA}((_?{ALNUM})+)*_)|({ALPHA}{ALNUM}*)

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
RCONST_D	{RCONST_D_1}|{RCONST_D_2}(E[+-]?{VALID_DNUM})?

/* Hexadecimal Real Constants */
RCONST_H_1	({VALID_HNUM}|0)?\.([0-9A-F]*[1-9A-F][0-9A-F]*|0)
RCONST_H_2	{VALID_HNUM}(\.(([0-9A-F]*[1-9A-F][0-9A-F]*)|0)?)?
RCONST_H	0H({RCONST_H_1}|{RCONST_H_2})

/* Binary Real Constants */
RCONST_B_1	({VALID_BNUM}|0)?\.([01]*1[01]*|0)
RCONST_B_2	{VALID_BNUM}(\.(([01]*1[01]*)|0)?)?
RCONST_B	0B({RCONST_B_1}|{RCONST_B_2})

/* Logical Constant */
LCONST \.(TRUE|FALSE)\.

/* Characters */
CHARACTER '((([$-~]| ))|(\\[nftrbv]))'

/* List Functions */
LISTFUNC C(AD*|D+)R

%%
 /*------------ RULES SECTION ---------------*/


<COMMENT>{
\n		{ printf("End of COMMENT\n"); BEGIN(INITIAL);}
[^\n]+
}

<INITIAL>{
"$"			{ printf("Start of COMMENT\n"); BEGIN(COMMENT);}

<<EOF>>			{ printf("Found EOF\n"); return T_EOF;}
{WHITECHAR}		{}

 /* Keywords */
"FUNCTION"		{ printf("Found keyword \"%s\"\n", yytext);	return T_FUNCTION; }
"SUBROUTINE"	{ printf("Found keyword \"%s\"\n", yytext);	return T_SUBROUTINE; }
"END"			{ printf("Found keyword \"%s\"\n", yytext);	return T_END; }
"INTEGER"		{ printf("Found keyword \"%s\"\n", yytext);	return T_INTEGER; }
"REAL"			{ printf("Found keyword \"%s\"\n", yytext);	return T_REAL; }
"LOGICAL"		{ printf("Found keyword \"%s\"\n", yytext);	return T_LOGICAL; }
"CHARACTER"		{ printf("Found keyword \"%s\"\n", yytext);	return T_CHARACTER; }
"COMPLEX"		{ printf("Found keyword \"%s\"\n", yytext);	return T_COMPLEX; }
"RECORD"		{ printf("Found keyword \"%s\"\n", yytext);	return T_RECORD; }
"ENDREC"		{ printf("Found keyword \"%s\"\n", yytext);	return T_ENDREC; }
"LIST"			{ printf("Found keyword \"%s\"\n", yytext);	return T_LIST; }  
"DATA"			{ printf("Found keyword \"%s\"\n", yytext);	return T_DATA; }
"CONTINUE"		{ printf("Found keyword \"%s\"\n", yytext);	return T_CONTINUE; }
"GOTO"			{ printf("Found keyword \"%s\"\n", yytext);	return T_GOTO; }
"CALL"			{ printf("Found keyword \"%s\"\n", yytext);	return T_CALL; }
"READ"			{ printf("Found keyword \"%s\"\n", yytext);	return T_READ; }
"WRITE"			{ printf("Found keyword \"%s\"\n", yytext);	return T_WRITE; }
"NEW"			{ printf("Found keyword \"%s\"\n", yytext);	return T_NEW; }
"LENGTH"		{ printf("Found keyword \"%s\"\n", yytext);	return T_LENGTH;}
"IF"			{ printf("Found keyword \"%s\"\n", yytext);	return T_IF; }
"THEN"			{ printf("Found keyword \"%s\"\n", yytext);	return T_THEN; }
"ELSE"			{ printf("Found keyword \"%s\"\n", yytext);	return T_ELSE; }
"ENDIF"			{ printf("Found keyword \"%s\"\n", yytext);	return T_ENDIF; }
"DO"			{ printf("Found keyword \"%s\"\n", yytext);	return T_DO; }
"ENDDO"			{ printf("Found keyword \"%s\"\n", yytext);	return T_ENDDO; }
"STOP"			{ printf("Found keyword \"%s\"\n", yytext);	return T_STOP; }
"RETURN"		{ printf("Found keyword \"%s\"\n", yytext);	return T_RETURN; }

 /* Identifier */
{ID}			{ printf("Found id \"%s\"\n", yytext); }

 /* Iconst */
{ICONST_D}		{ printf("Found integer: %d  as \"%s\"\n", atoi(yytext), yytext); return T_ICONST; }
{ICONST_H}		{ printf("Found integer: %ld as \"%s\"\n", GET_INT_VALUE(yytext, 16), yytext); return T_ICONST; }
{ICONST_B}		{ printf("Found integer: %ld as \"%s\"\n", GET_INT_VALUE(yytext, 2), yytext); return T_ICONST; }

 /* Rconst */
{RCONST_D}		{ printf("Found real: %lf as \"%s\"\n", strtod(yytext, NULL), yytext); return T_RCONST; }
{RCONST_H}		{ printf("Found real: %lf as \"%s\"\n", GET_REAL_VALUE(yytext, 16), yytext); return T_RCONST; }
{RCONST_B}		{ printf("Found real: %lf as \"%s\"\n", GET_REAL_VALUE(yytext, 2), yytext); return T_RCONST; }


 /* Operators */
".AND."					{ printf("Found operator \"%s\"\n", yytext); return T_ANDOP; }
".OR."					{ printf("Found operator \"%s\"\n", yytext); return T_OROP; }
".NOT."					{ printf("Found operator \"%s\"\n", yytext); return T_NOTOP; }
"."([GL][TE]|EQ|NE)"."	{ printf("Found operator \"%s\"\n", yytext); return T_RELOP; }
"+"|"-"					{ printf("Found operator \"%s\"\n", yytext); return T_ADDOP; }
"*"						{ printf("Found operator \"%s\"\n", yytext); return T_MULOP; }
"/"						{ printf("Found operator \"%s\"\n", yytext); return T_DIVOP; }
"**"					{ printf("Found operator \"%s\"\n", yytext); return T_POWEROP; }

/* characters */
{CHARACTER}            { printf("Found Character "%c"\n", yytext[1]);	return T_CHARACTER; }

/* List Functions */
{LISTFUNC}		{ printf("Found list function: \"%s\"\n", yytext); return T_LISTFUNC; }

.				{printf("Unknown \"%s\"\n", yytext);}
}
%%
/*------------ USER-CODE SECTION ------------*/

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

	do {
		token = yylex();
	} while (token != 0);

	fclose(yyin);
	yyterminate();
}