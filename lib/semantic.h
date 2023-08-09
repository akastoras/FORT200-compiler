#ifndef _SEMANTIC_H_ 
#define _SEMANTIC_H_

#include "ast.h"

void SEM_signable_constant(AST_Sign, AST_Constant *);
void SEM_typecheck_initVal(char *, AST_Values *);



#endif