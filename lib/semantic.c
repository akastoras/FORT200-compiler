#include "ast.h"
#include "../constants.h"
#include "symbol_table.h"
#include <assert.h>
#include <stdio.h>

extern HASHTBL *symbol_table;
extern int scope;

// Ensure that a constant has a sign only if it is int or real
int SEM_signable_constant(AST_Sign sign, AST_Constant *constant)
{
	if (sign != NONE) {
		if (constant->type != INT && constant->type != REAL && constant->type != CMPLX) {
			yyerror("<SEM> Incompatible sign with type.");
			return 1;
		}
	}
	return 0;
}

// Ensure that a variable has been declared
int SEM_check_existing_variable(decl_t *decl, char *id)
{
	char buffer[MAX_STRING_LENGTH];
	if (decl == NULL) {
		sprintf(buffer, "<SEM> Variable '%s' has not been declared", id);
		yyerror(buffer);
		return 1;
	}
	return 0;
}

// Ensure that a variable has been initialized
int SEM_check_initial_value_exists(decl_t *decl)
{
	char buffer[MAX_STRING_LENGTH];
	assert(decl != NULL);
	if (decl->initial_value == NULL) {
		sprintf(buffer, "<SEM> Variable '%s' not initialized before use", decl->variable->id);
		yyerror(buffer);
		return 1;
	}
	
	return 0;
}

// Ensure that a variable used in a certain context has the proper type
int SEM_check_decl_datatype_simple(AST_GeneralType *datatype, type_t type, char *id)
{
	char buffer[MAX_STRING_LENGTH];
	assert(datatype != NULL);
	if (datatype->type != type) {
		sprintf(buffer, "<SEM> Variable '%s' is of incompatible type", id);
		yyerror(buffer);
		return 1;
	}
	return 0;
}

int SEM_check_compatible_initialization(AST_GeneralType *datatype, AST_Values *value_list)
{
	// TODO: implement this
	return 0;
}