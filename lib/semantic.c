#include "ast.h"
#include "../constants.h"

extern HASHTBL *symbol_table;
extern int scope;

// Make sure that a constant has a sign only if it is int or real
void SEM_signable_constant(AST_Sign sign, AST_Constant *constant)
{
	if (sign != NONE) {
		if (constant->type != INT && constant->type != REAL && constant->type != CMPLX) {
			yyerror("<SEM> Incompatible sign with type.");
		}
	}
}

// 
void SEM_typecheck_initVal(char *id, AST_Values *value_list)
{
	return;
}
