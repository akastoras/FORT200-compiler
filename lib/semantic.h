#ifndef _SEMANTIC_H_ 
#define _SEMANTIC_H_

#include "ast.h"
#include "symbol_table.h"
#include <stdint.h>

// Colors
#define RED     "\033[31m"      /* Red */
#define RESET   "\033[0m"

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

// Enum for SEM_check_variable_type
// passing several options as a single integer
enum var_options {
	O_ID			= 0x01,	// 0000001
	O_DECL			= 0x02,	// 0000010
	O_FUNC_CALL		= 0x04,	// 0000100
	O_ARRAY_ACCESS	= 0x08,	// 0001000
	O_REC_ACCESS	= 0x10, // 0010000
	O_LISTFUNC		= 0x20,	// 0100000
};

void SEM_signable_constant(AST_Sign, AST_Constant *);
void SEM_check_existing_variable(decl_t *, char *);
void SEM_check_initial_value_exists(decl_t *);
void SEM_check_decl_datatype_simple(AST_GeneralType *, type_t, char *);
void SEM_check_compatible_initialization(decl_t *, AST_Values *);
void SEM_check_list_depth(AST_UndefVar *);
void SEM_check_duplicate_variable_name(const char *);
void SEM_check_initialization_position();
void SEM_check_duplicate_subprogram_name(char *);
void SEM_check_existing_arguments(AST_Params *, char *);
void SEM_check_declared_dims(AST_Dims *, bool);
void SEM_check_existing_record_field(AST_Field *field, char *rec_id, char *field_id);
void SEM_check_expr_datatype(AST_Expression *expr, uint8_t options, bool compatible);
void SEM_check_variable_type(AST_Variable *variable, uint8_t options);
void SEM_check_expression_list(AST_Expression *expr);
void SEM_check_expression_not_list(AST_Expression *expr);
void SEM_check_expression_not_array(AST_Expression *expr);
void SEM_check_same_datatypes(AST_GeneralType *datatype1, AST_GeneralType *datatype2);
void SEM_check_same_list_depth(int list_depth1, int list_depth2);
void SEM_check_valid_listexpr_hops(AST_Expressions *listexpr, int hops);
void SEM_check_valid_array_access(AST_UndefVar *undef_var, AST_Expressions *exprs);
void SEM_check_possible_types(unmatched_expr_use_t *expr_use, type_t type);
void SEM_check_subprog_call_exists(AST_Subprogram *subprogram, char *id);
void SEM_check_subprogram_type(unmatched_expr_use_t *expr_use, AST_Subprogram *subprogram);
void SEM_check_func_call_params(AST_Params *params, AST_Expressions *exprs, char *id);


// Typechecking for variables
void SEM_typecheck_variable(AST_Variable *, type_t , char *string);


#endif