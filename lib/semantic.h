#ifndef _SEMANTIC_H_ 
#define _SEMANTIC_H_

#include "ast.h"
#include "symbol_table.h"

void SEM_signable_constant(AST_Sign, AST_Constant *);
void SEM_check_existing_variable(decl_t *);
void SEM_check_initial_value_exists(decl_t *);
void SEM_check_decl_datatype_simple(AST_GeneralType *, type_t, char *);

#endif