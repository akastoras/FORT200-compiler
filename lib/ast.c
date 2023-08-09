/****************************************************/
/************** AST IMPLEMENTATION ******************/
/****************************************************/
#include "ast.h"
#include "semantic.h"
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

// Malloc with safety
void *safe_realloc(void *ptr, size_t size)
{
	void *new_ptr = realloc(ptr,size);
	if (new_ptr == NULL) {
		perror("Internal error.");
		exit(EXIT_FAILURE);
	}
	return new_ptr;
}

/****************************************************/
/************** FUNCTIONS FOR CONSTANTS *************/
/****************************************************/


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
AST_Constant *ast_get_CMPLX(double re, AST_Sign im_sign, double im)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->value.cmplxval.re = re;
	if (im_sign != NONE) {
		ret_val->value.cmplxval.im = im * im_sign;
	}
	else {
		ret_val->value.cmplxval.im = im;
	}
	ret_val->type = CMPLX;

	printf("(%f:%f)\n", ret_val->value.cmplxval.re, ret_val->value.cmplxval.im);

	return ret_val;
}

/****** Functions for Sign ******/

// Create ast node for sign
AST_Sign ast_get_sign(AST_Sign sign)
{
	return sign;
}

// Create ast node for constant value
AST_Constant *ast_get_value(AST_Sign sign, AST_Constant *constant)
{
	// Check if the constant and the sign are valid
	SEM_signable_constant(sign, constant);
	
	// Update the constant struct if the sign is minus
	if (sign == MINUS) {
		if (constant->type == INT) {
			constant->value.intval = -constant->value.intval;
		}
		else if (constant->type == REAL) {
			constant->value.rval = -constant->value.rval;
		}
		else {
			constant->value.cmplxval.re = -constant->value.cmplxval.re;
			constant->value.cmplxval.im = -constant->value.cmplxval.im;
		}
	}

	return constant;
}

// Create ast node for string value
AST_Constant *ast_get_string(char *strval)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->value.strval = strval;
	ret_val->type = STR;
	
	return ret_val;
}

AST_Values *ast_insert_value_to_values(AST_Values *values, AST_Constant *value)
{
	AST_Values *new_values;

	if (values == NULL) {
		// Case of the first value
		new_values = safe_malloc(sizeof(AST_Values));
		new_values->size = 1;
		new_values->data = NULL;
	}
	else {
		// Case of the other values
		new_values = values;
		new_values->size++;
	}

	// Extend the array with the new element
	new_values->data = safe_realloc(new_values->data, new_values->size * sizeof(AST_Constant *));
	new_values->data[new_values->size - 1] = value;

	return new_values;
}

AST_Vals *ast_insert_val_to_vals(AST_Vals *vals, char *id, AST_Values *value_list)
{
	AST_Vals *new_vals;

	SEM_typecheck_initVal(char *id, AST_Values *value_list);

	if(vals == NULL) {
		// Case of the first value
		new_vals = safe_malloc(sizeof(AST_Vals));
		new_vals->size = 1;
		new_vals->elements = NULL;
	}
	else {
		// Case of the other values
		new_vals = vals;
		new_vals->size++;
	}
	
	// Extend pointer array
	new_vals->elements = safe_realloc(new_vals->elements, new_vals->size * sizeof(AST_InitVal *));
	// Allocate memory for InitVal struct
	new_vals->elements[new_vals->size - 1] = safe_malloc(sizeof(AST_InitVal));
	
	// Update the struct fields
	new_vals->elements[new_vals->size - 1]->id = id;
	new_vals->elements[new_vals->size - 1]->value_list = value_list;

	return new_vals;
}







// Print an AST_Values structure
void ast_print_values(AST_Values *values)
{
	printf("#-----Values-----#\n");
	for (int i = 0; i < values->size; i++) {
		AST_Constant *constant = values->data[i];
		switch (constant->type) {
			case INT:
				printf("Integer value %d\n", constant->value.intval); break;
			case LOG:
				printf("Logical value %s\n", (constant->value.lval) ? "True" : "False"); break;
			case REAL:
				printf("Real value %lf\n", constant->value.rval); break;
			case CHAR:
				printf("Character value %c\n", constant->value.charval); break;
			case STR:
				printf("String value %s\n", constant->value.strval); break;
			case CMPLX:
				printf("Complex value with Re = %lf and Im = %lf\n",
					constant->value.cmplxval.re, constant->value.cmplxval.im); break;
			default:
				printf("Value of unknown type\n");
		}
	}
	printf("#------------------#\n");
}