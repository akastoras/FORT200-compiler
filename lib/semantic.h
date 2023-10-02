#ifndef _SEMANTIC_H_ 
#define _SEMANTIC_H_

#include "ast.h"
#include "symbol_table.h"
#include <stdint.h>

// Enum for SEM_check_expr_datatype
// passing several options as a single integer
enum options {
	O_INTEGER	= 0x01,	// 0000001
	O_LOGICAL	= 0x02,	// 0000010
	O_REAL		= 0x04,	// 0000100
	O_CHARACTER	= 0x08,	// 0001000
	O_STRING	= 0x10, // 0010000
	O_COMPLEX	= 0x20,	// 0100000
	O_RECORD	= 0x40,	// 1000000
};

int SEM_signable_constant(AST_Sign, AST_Constant *);
int SEM_check_existing_variable(decl_t *, char *);
int SEM_check_initial_value_exists(decl_t *);
int SEM_check_decl_datatype_simple(AST_GeneralType *, type_t, char *);
int SEM_check_compatible_initialization(decl_t *, AST_Values *);
int SEM_check_list_depth(AST_UndefVar *);
int SEM_check_duplicate_variable_name(const char *);
int SEM_check_initialization_position();
int SEM_check_duplicate_subprogram_name(char *);
int SEM_check_existing_arguments(AST_Params *, char *);
int SEM_check_declared_dims(AST_Dims *, bool);
int SEM_check_existing_record_field(AST_Field *field, char *rec_id, char *field_id);
int SEM_check_expr_datatype(AST_Expression *expr, uint8_t);
int SEM_check_expression_list(AST_Expression *expr);
int SEM_check_expression_not_list(AST_Expression *expr);
int SEM_check_expression_not_array(AST_Expression *expr);
int SEM_check_same_datatypes(AST_GeneralType *datatype1, AST_GeneralType *datatype2);
int SEM_check_same_list_depth(int list_depth1, int list_depth2);

// Typechecking for variables
int SEM_typecheck_variable(AST_Variable *, type_t , char *string);


#endif