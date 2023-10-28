#include "semantic.h"
#include "ast.h"
#include "../constants.h"
#include "symbol_table.h"
#include <assert.h>
#include <stdio.h>
#include <stdint.h>


extern HASHTBL *symbol_table;
extern int scope;
extern char *type_str[];
extern char *var_type_str[];

void SEM_error(char *buffer)
{
	printf(RED "SEM ERROR: " RESET "%s\n", buffer);
	exit(1);
}

// Ensure that a constant has a sign only if it is int or real
void SEM_signable_constant(AST_Sign sign, AST_Constant *constant)
{
	char buffer[MAX_STRING_LENGTH];
	if (sign != NONE) {
		if (constant->type != INT && constant->type != REAL &&
			constant->type != CMPLX) {
			sprintf(buffer, "Incompatible sign with type");
			SEM_error(buffer);
		}
	}
}

// Ensure that a variable has been declared
void SEM_check_existing_variable(decl_t *decl, char *id)
{
	char buffer[MAX_STRING_LENGTH];
	if (decl == NULL) {
		sprintf(buffer, "Variable '%s' has not been declared", id);
		SEM_error(buffer);
	}
}

// Ensure that a variable has been initialized
void SEM_check_initial_value_exists(decl_t *decl)
{
	char buffer[MAX_STRING_LENGTH];
	assert(decl != NULL);
	if (decl->initial_value == NULL) {
		sprintf(buffer, "Variable '%s' not initialized before use",
														decl->variable->id);
		SEM_error(buffer);	
	}
}

// Ensure that a variable used in a certain context has the proper type
void SEM_check_decl_datatype_simple(AST_GeneralType *datatype, type_t type, char *id)
{
	char buffer[MAX_STRING_LENGTH];
	assert(datatype != NULL);
	if (datatype->type != type) {
		sprintf(buffer, "Variable '%s' is of incompatible type", id);
		SEM_error(buffer);
	}
}

void SEM_check_compatible_initialization(decl_t *decl, AST_Values *value_list)
{ return; }

// Ensure that the depth of a list is less than the acceptable
void SEM_check_list_depth(AST_UndefVar *list_node)
{
	char buffer[MAX_STRING_LENGTH];

	if (list_node->list_depth > MAX_LIST_DEPTH) {
		sprintf(buffer, "List %s has depth more than the acceptable %d",
													list_node->id, MAX_LIST_DEPTH);
		SEM_error(buffer);
	}
}

// Check if id has already been used
void SEM_check_duplicate_variable_name(const char *new_id)
{
	STBL_Entry *var = stbl_search_current_scope(new_id);
	char buffer[MAX_STRING_LENGTH];

	if (var != NULL) {
		sprintf(buffer, "Variable %s has already been declared", new_id);
		SEM_error(buffer);
	}
}

// Ensure that no declarations happen 
void SEM_check_initialization_position()
{
	char buffer[MAX_STRING_LENGTH];

	if (stbl_get_curr_scope() != 1) {
		sprintf(buffer, "Initialization allowed at the outermost scope of a unit");
		SEM_error(buffer);
	}
}

// Make sure that there is no other subprogram with the same name
void SEM_check_duplicate_subprogram_name(char *id)
{
	AST_Subprogram *subprogram = stbl_search_subprogram(id);
	char buffer[MAX_STRING_LENGTH];

	if (subprogram != NULL) {
		sprintf(buffer, "Subprogram %s has already been declared", id);
		SEM_error(buffer);
	}
}

// Check that the parameters of a function have at least one parameter
void SEM_check_existing_arguments(AST_Params *params, char *id)
{
	char buffer[MAX_STRING_LENGTH];
	
	if (params == NULL || params->size < 1) {
		sprintf(buffer, "Function %s requires at least one parameters.", id);
		SEM_error(buffer);
	}
}

void check_dim_variable(AST_Dim *dim, bool is_param)
{
	char *id = dim->id;
	decl_t *decl = stbl_search_variable(id);

	SEM_check_existing_variable(decl, id);
	SEM_check_decl_datatype_simple(decl->datatype, INT, id);
	
	// Local array cannot have a parameter as a dimension
	if (decl->is_parameter && !is_param) {
		char buffer[MAX_STRING_LENGTH];
		sprintf(buffer, "Local array cannot have parameter '%s' as a dimension", id);
		SEM_error(buffer);
	}
	else if (!(decl->is_parameter)) {
		/* When both variables are not parameters
			the dimension variable should be initialized */
		SEM_check_initial_value_exists(decl);
		dim->type = DIM_LITERAL;
		dim->val = decl->initial_value->elements[0]->intval;
	}
	
	dim->decl = decl;
}

// Ensure that variable dimensions of an array exist in the symbol table
void SEM_check_declared_dims(AST_Dims *dims, bool is_param)
{
	for (int i = 0; i < dims->size; i++) {
		if (dims->elements[i]->type == DIM_VARIABLE) {
			check_dim_variable(dims->elements[i], is_param);
		}
	}
}

// Ensure the field given exists inside the record with name rec_id
void SEM_check_existing_record_field(AST_Field *field, char *rec_id, char *field_id)
{
	char buffer[MAX_STRING_LENGTH];
	
	if (field == NULL) {
		sprintf(buffer, "Record %s does not contain field %s", rec_id, field_id);
		SEM_error(buffer);
	}
}

void SEM_check_expr_datatype(AST_Expression *expr, uint8_t options, bool compatible)
{
	char buffer[MAX_STRING_LENGTH];

	// Find the option value of the datatype of the expression
	// and check if it exists in the options
	
	if (!(options & (1 << expr->datatype->type))) {
		if (compatible) {
			sprintf(buffer,
			"Expression has type %s which is incompatible with that expression",
			type_str[expr->datatype->type]);
			SEM_error(buffer);
		}
		else {
			sprintf(buffer,
			"Type %s is not one of the possible types of the expresion",
			type_str[expr->datatype->type]);
			SEM_error(buffer);
		}
		
	}
}

void SEM_check_variable_type(AST_Variable *variable, uint8_t options)
{
	char buffer[MAX_STRING_LENGTH];

	// Find the option value of the variable type of the expression
	// and check if it exists in the options
	if (!(options & (1 << variable->type))) {
		sprintf(buffer,
			"Variable has type %s which is incompatible with that variable.",
			var_type_str[variable->type]);
		SEM_error(buffer);
	}
}

// Check if a variable is of a specific simple type
void SEM_typecheck_variable(AST_Variable *variable, type_t type, char *string)
{
	char buffer[MAX_STRING_LENGTH];

	if (variable->datatype->type != type) {
		sprintf(buffer, "%s is not of type %s", string, type_str[type]);
		SEM_error(buffer);
	}
}

// Check if a variable is a list
void SEM_check_expression_list(AST_Expression *expr)
{
	char buffer[MAX_STRING_LENGTH];

	if (expr->list_depth == 0) {
		sprintf(buffer, "Expression is not list");
		SEM_error(buffer);
	}
}

// Check if variable is not a list
void SEM_check_expression_not_list(AST_Expression *expr)
{
	char buffer[MAX_STRING_LENGTH];

	if (expr->list_depth != 0) {
		sprintf(buffer, "Expression is list");
		SEM_error(buffer);
	}
}

// Check if variable is not an array
void SEM_check_expression_not_array(AST_Expression *expr)
{
	char buffer[MAX_STRING_LENGTH];

	if (expr->dims != NULL) {
		sprintf(buffer, "Expression is array");
		SEM_error(buffer);
	}
}

/* At the moment it is only used to ensure a
  list expression has expressions of the same datatype */
void SEM_check_same_datatypes(AST_GeneralType *datatype1, AST_GeneralType *datatype2)
{
	char buffer[MAX_STRING_LENGTH];
	assert(datatype1 != NULL && datatype2 != NULL);
	if (datatype1->type != datatype2->type) {
		sprintf(buffer, "Datatypes are not the same");
		SEM_error(buffer);
	}
}

// Check if nested lists are of different depths
void SEM_check_same_list_depth(int list_depth1, int list_depth2)
{
	char buffer[MAX_STRING_LENGTH];
	
	if (list_depth1 != list_depth2) {
		sprintf(buffer, "Nested lists do not have the same depth");
		SEM_error(buffer);
	}
}

// Check if the number of hops in a listexpression exceeds the number of elements
void SEM_check_valid_listexpr_hops(AST_Expressions *listexpr, int hops)
{
	char buffer[MAX_STRING_LENGTH];

	if(hops > listexpr->size) {
		sprintf(buffer, "Access to list expression exceeds list's bounds");
		SEM_error(buffer);
	}
}

void SEM_check_valid_array_access(AST_UndefVar *undef_var, AST_Expressions *exprs)
{
	char buffer[MAX_STRING_LENGTH];

	// Check that the given declaration is of an array variable
	if (undef_var->type != ARRAY) {
		sprintf(buffer, "Variable %s is not an array", undef_var->id);
		SEM_error(buffer);
	}
	
	// Check that the number of expressions matches the number of dims
	if (undef_var->dims->size != exprs->size) {
		sprintf(buffer, "Access to array %s has incorrect number of dimensions",
				undef_var->id);
		SEM_error(buffer);
	}

	/* Ensure all expressions with defined type are integers. If there are any
	   unmatched expressions, give them an integer expected datatype */
	for (int i = 0; i < exprs->size; i++) {
		AST_Expression *expr = exprs->elements[i];
		if (expr->expr_type != EXPR_LISTEXPR && expr->datatype->type == AMBIGUOUS) {
			impose_constraint_recursively(expr, O_INTEGER);
			continue;
		}

		switch (expr->expr_type)
		{
		case EXPR_UNARY:
			// Unary expression used as a dim. Ensure it has an integer type
			if (expr->datatype->type != INT) {
				sprintf(buffer, "Non-integer unary expression "
					"used as a dimension to access array %s",
					undef_var->id);
				SEM_error(buffer);
			}

			break;
		case EXPR_BINARY:
			// Binary expression used as a dim. Ensure it has an integer type
			if (expr->datatype->type != INT) {
				sprintf(buffer, "Non-integer binary expression "
					"used as a dimension to access array %s",
					undef_var->id);
				SEM_error(buffer);
			}

			break;
		case EXPR_VARIABLE:
			switch (expr->variable->type)
			{
			case V_ID:
				// Error in the handling of variables
				sprintf(buffer, "Variable expression of ID variable type "
						"used as a dimension to access array %s",
						undef_var->id);
				SEM_error(buffer);

				break;
			case V_DECL:
				// Scalar variable used as a dim. Ensure it has an integer type
				if (expr->datatype->type != INT) {
					sprintf(buffer, "Non-integer scalar variable expression "
						"used as a dimension to access array %s",
						undef_var->id);
					SEM_error(buffer);
				}

				break;
			case V_FUNC_CALL:
				sprintf(buffer, "Function call cannot have a non-ambiguous type "
						"at this moment %s", undef_var->id);
				SEM_error(buffer);

				break;
			case V_ARRAY_ACCESS:
				// The array being accessed should have integer values
				if (expr->datatype->type != INT) {
					sprintf(buffer, "Non-integer array variable expression "
						"used as a dimension to access array %s",
						undef_var->id);
					SEM_error(buffer);
				}

				break;
			case V_REC_ACCESS:
				// The field being accessed should have integer values
				if (expr->datatype->type != INT) {
					sprintf(buffer, "Non-integer record field variable expression "
						"used as a dimension to access array %s",
						undef_var->id);
					SEM_error(buffer);
				}

				break;
			case V_LISTFUNC:
				// Only an accessing list function used on a list of integers is allowed
				if (expr->variable->listfunc->access != true ||
					expr->variable->list_depth != 0 ||
					expr->datatype->type != INT) {
					sprintf(buffer, "Only accessing list functions used on lists of "
						"integers can be used as a dimension to access array %s",
						undef_var->id);
					SEM_error(buffer);
				}
				break;
			default:
				// Unknown type of variable
				sprintf(buffer, "Unknown type of variable expression used as "
						"a dimension to access array %s", undef_var->id);
				SEM_error(buffer);

				break;
			}
			break;
		case EXPR_CONSTANT:
			// Every constant expr used as a dim should be an integer
			if (expr->constant->type != INT) {
				sprintf(buffer, "Constant non-integer expression "
						"used as a dimension to access array %s", undef_var->id);
				SEM_error(buffer);
			}

			break;
		case EXPR_LISTEXPR:
			// List expression cannot be used as a dim
			sprintf(buffer, "List expression used as a dimension "
						"to access array %s", undef_var->id);
			SEM_error(buffer);

			break;
		default:
			// Unknown type of expression
			sprintf(buffer, "Unknown type of expression used as a dimension "
						"to access array %s", undef_var->id);
			SEM_error(buffer);
			break;
		}
	}
}

void SEM_check_possible_types(unmatched_expr_use_t *expr_use, type_t type)
{
	if ((expr_use->possible_types & (1 << type)) == 0) {
		char buffer[MAX_STRING_LENGTH];
		sprintf(buffer, "Ambiguous expression cannot be of type %s",
				type_str[type]);
		SEM_error(buffer);
	}
}

void SEM_check_subprog_call_exists(AST_Subprogram *subprogram, char *id)
{
	if (subprogram == NULL) {
		char buffer[MAX_STRING_LENGTH];
		sprintf(buffer, "Subprogram with id %s does not exist", id);
		SEM_error(buffer);
	}
}

void SEM_check_subprogram_type(unmatched_expr_use_t *expr_use, AST_Subprogram *subprogram)
{	
	if (expr_use->is_subroutine == false) {
		if (subprogram->header->subprogram_type == SUBROUTINE) {
			char buffer[MAX_STRING_LENGTH];
			sprintf(buffer, "Subroutine %s cannot have a return value",
					expr_use->expr->variable->id);
			SEM_error(buffer);
		}
	} 
	else {
		if (subprogram->header->subprogram_type == FUNCTION) {
			char buffer[MAX_STRING_LENGTH];
			sprintf(buffer, "Function %s cannot be identified as a subroutine",
					expr_use->expr->variable->id);
			SEM_error(buffer);
		}
	}
}

void SEM_check_func_call_params(AST_Params *params, AST_Expressions *exprs, char *id)
{
	char buffer[MAX_STRING_LENGTH];

	if (params->size != exprs->size) {
		sprintf(buffer, "Call of function %s has incorrect number of parameters", id);
		SEM_error(buffer);
	}

	for (int i = 0; i < params->size; i++) {
		if (params->elements[i]->datatype->type != exprs->elements[i]->datatype->type) {
			sprintf(buffer, "Call of function %s has a parameter "
					"of incorrect type at position %d", id, i+1);
			SEM_error(buffer);
		}

		switch (exprs->elements[i]->expr_type)
		{
		case EXPR_CONSTANT:
		case EXPR_BINARY:
		case EXPR_UNARY:
		case EXPR_LISTEXPR:
			if(params->elements[i]->variable->dims != NULL) {
				sprintf(buffer, "Call of function %s has a scalar parameter "
						"at position %d, while it should be an array", id, i+1);
				SEM_error(buffer);
			}

			break;
		case EXPR_VARIABLE:
			if(params->elements[i]->variable->dims != NULL) {
				if (exprs->elements[i]->variable->dims == NULL) {
					sprintf(buffer, "Call of function %s has a scalar parameter "
							"at position %d, while it should be an array", id, i+1);
					SEM_error(buffer);
				}

				if (params->elements[i]->variable->dims->size != exprs->elements[i]->dims->size) {
					sprintf(buffer, "Call of function %s has an array parameter "
							"at position %d with incorrect number of dims", id, i+1);
					SEM_error(buffer);
				}	
			}
			else {
				if (exprs->elements[i]->variable->type != V_ARRAY_ACCESS &&
					exprs->elements[i]->variable->dims != NULL) {
					sprintf(buffer, "Call of function %s has an array parameter "
							"at position %d, while it should be a scalar", id, i+1);
					SEM_error(buffer);
				}
			}

			break;
		default:
			break;
		}
	}
}