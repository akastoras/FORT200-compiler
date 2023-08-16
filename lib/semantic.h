#ifndef _SEMANTIC_H_ 
#define _SEMANTIC_H_

#include "ast.h"
#include "symbol_table.h"

int SEM_signable_constant(AST_Sign, AST_Constant *);
int SEM_check_existing_variable(decl_t *, char *);
int SEM_check_initial_value_exists(decl_t *);
int SEM_check_decl_datatype_simple(AST_GeneralType *, type_t, char *);
int SEM_check_compatible_initialization(decl_t *, AST_Values *);

#endif