#ifndef _TYPES_H_
#define _TYPES_H_

#include <stdbool.h>

/****************************************************/
/********************** STRUCTS *********************/
/****************************************************/

// Value representing the type of an expression
typedef enum {
	INT, LOG, REAL, CHAR, STR, CMPLX
} type_t;

// Representation of complex as two reals
typedef struct {
	double re;
	double im;
} complex_t;

// Struct representing a generic expression's type and value 
typedef struct {
	// Value of the expression, accessing the field
	// dictated by the type
	union  {
		int  intval;
		bool  lval;
		double rval;
		char charval;
		char *strval;
		complex_t cmplxval;
	} value;
	// Type of the expression's result
	type_t type;
} AST_Constant;

typedef type_t AST_Type;


typedef enum {POSITIVE, NEGATIVE, NONE} AST_Sign;


/****************************************************/
/********************* FUNCTIONS ********************/
/****************************************************/

AST_Type *ast_get_type_int();
AST_Type *ast_get_type_real();
AST_Type *ast_get_type_logical();
AST_Type *ast_get_type_character();
AST_Type *ast_get_type_complex();

AST_Constant *ast_get_ICONST(int val);
AST_Constant *ast_get_RCONST(double val);
AST_Constant *ast_get_CCONST(char val);
AST_Constant *ast_get_LCONST(bool val);
AST_Constant *ast_get_CMPLX(double re, double im);

AST_Sign *ast_get_sign(char *sign_str);



#endif