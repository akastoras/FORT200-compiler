/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_SYNTAX_TAB_H_INCLUDED
# define YY_YY_SYNTAX_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    T_EOF = 0,
    T_FUNCTION = 258,
    T_SUBROUTINE = 259,
    T_END = 260,
    T_INTEGER = 261,
    T_REAL = 262,
    T_LOGICAL = 263,
    T_CHARACTER = 264,
    T_COMPLEX = 265,
    T_RECORD = 266,
    T_ENDREC = 267,
    T_LIST = 268,
    T_DATA = 269,
    T_CONTINUE = 270,
    T_GOTO = 271,
    T_CALL = 272,
    T_READ = 273,
    T_WRITE = 274,
    T_NEW = 275,
    T_LENGTH = 276,
    T_IF = 277,
    T_THEN = 278,
    T_ELSE = 279,
    T_ENDIF = 280,
    T_DO = 281,
    T_ENDDO = 282,
    T_STOP = 283,
    T_RETURN = 284,
    T_ID = 285,
    T_ICONST = 286,
    T_RCONST = 287,
    T_LCONST = 288,
    T_CCONST = 289,
    T_OROP = 290,
    T_ANDOP = 291,
    T_NOTOP = 292,
    T_RELOP = 293,
    T_ADDOP = 294,
    T_MULOP = 295,
    T_DIVOP = 296,
    T_POWEROP = 297,
    T_LISTFUNC = 298,
    T_STRING = 299,
    T_LPAREN = 300,
    T_RPAREN = 301,
    T_COMMA = 302,
    T_ASSIGN = 303,
    T_DOT = 304,
    T_COLON = 305,
    T_LBRACK = 306,
    T_RBRACK = 307
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 18 "syntax.y"

	int 		intval;
	float		floatval;
	long 		lval;
	long double	rval;
	char		charval;

#line 119 "syntax.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_SYNTAX_TAB_H_INCLUDED  */
