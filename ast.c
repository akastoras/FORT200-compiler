#include "ast.h"
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

// Malloc with safety
void *safe_malloc(size_t size)
{
	void *ptr = malloc(size);
	if (ptr == NULL) {
		perror("Internal error.");
		exit(EXIT_FAILURE);
	}
	return ptr;
}

/****************************************************/
/************** FUNCTIONS FOR CONSTANTS *************/
/****************************************************/


/****** Functions for Types ******/

// Create node for type int
AST_Type *ast_get_type_int()
{
	AST_Type *ret_val = safe_malloc(sizeof(AST_Type));
	*ret_val = INT;

	return ret_val;
}

// Create node for type real
AST_Type *ast_get_type_real()
{
	AST_Type *ret_val = safe_malloc(sizeof(AST_Type));
	*ret_val = REAL;
	
	return ret_val;
}

// Create node for type logical
AST_Type *ast_get_type_logical()
{
	AST_Type *ret_val = safe_malloc(sizeof(AST_Type));
	*ret_val = LOG;
	
	return ret_val;
}

// Create node for type character
AST_Type *ast_get_type_character()
{
	AST_Type *ret_val = safe_malloc(sizeof(AST_Type));
	*ret_val = CHAR;
	
	return ret_val;
}

// Create node for type complex
AST_Type *ast_get_type_complex()
{
	AST_Type *ret_val = safe_malloc(sizeof(AST_Type));
	*ret_val = CMPLX;
	
	return ret_val;
}


/****** Functions for Constants ******/

// Create ast node for integer constant
AST_Constant *ast_get_ICONST(int val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->value.intval = val;
	ret_val->type = INT;

	return ret_val;
}

// Create ast node for real constant
AST_Constant *ast_get_RCONST(double val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->value.rval = val;
	ret_val->type = REAL;

	return ret_val;
}

// Create ast node for character constant
AST_Constant *ast_get_CCONST(char val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->value.charval = val;
	ret_val->type = CHAR;

	return ret_val;
}

// Create ast node for logical constant
AST_Constant *ast_get_LCONST(bool val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->value.lval = val;
	ret_val->type = LOG;

	return ret_val;
}

// Create ast node for logical constant
AST_Constant *ast_get_CMPLX(double re, double im)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->value.cmplxval.re = re;
	ret_val->value.cmplxval.im = im;
	ret_val->type = CMPLX;

	return ret_val;
}

/****** Functions for Sign ******/

// Create ast node for sign
AST_Sign *ast_get_sign(char *sign_str)
{
	AST_Sign *ret_val = safe_malloc(sizeof(AST_Sign));
	
	if (sign_str == NULL) {
		*ret_val = NONE;
	}
	else if (sign_str[0] == '+') {
		*ret_val = POSITIVE;
	}
	else {
		*ret_val = NEGATIVE;
	}

	return ret_val;
}