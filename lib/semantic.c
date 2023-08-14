#include "ast.h"
#include "../constants.h"
#include "symbol_table.h"

extern HASHTBL *symbol_table;
extern int scope;

// Ensure that a constant has a sign only if it is int or real
void SEM_signable_constant(AST_Sign sign, AST_Constant *constant)
{
	if (sign != NONE) {
		if (constant->type != INT && constant->type != REAL && constant->type != CMPLX) {
			yyerror("<SEM> Incompatible sign with type.");
		}
	}
}

// Ensure that a variable has been declared
void SEM_check_existing_variable(decl_t *decl)
{
	if (decl == NULL) {
		yyerror("<SEM> Variable has not been declared")
	}
}

// Ensure that a variable has been initialized
void SEM_check_initial_value_exists(decl_t *decl)
{
	char buffer[MAX_STRING_LENGTH];
	assert(decl != NULL);
	if (decl->initial_value == NULL) {
		sprintf(buffer, "<SEM> Variable '%s' not initialized before use", decl->variable->id);
		yyerror(buffer);
		return;
	}
	assert(decl->initial_value->value_list != NULL);
}

// Ensure that a variable used in a certain context has the proper type
void SEM_check_decl_datatype_simple(AST_GeneralType *datatype, type_t type, char *id)
{
	char buffer[MAX_STRING_LENGTH];
	assert(datatype != NULL);
	if (datatype->type != type) {
		sprintf(buffer, "<SEM> Variable '%s' is of incompatible type", id);
		yyerror(buffer);
	}
}
