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

int recursive_SEM_check_REC_compatible_initialization(AST_Field **parent_field_elements, int parent_field_size, AST_Values *value_list, int *value_list_remainder)
{
	int i,j;
	
	for (i = 0; i < parent_field_size && *value_list_remainder > 0; i++) {
		AST_Field *field = parent_field_elements[i]; // Can contain multiple variables

		if (field->type != REC) {
			char buffer[MAX_STRING_LENGTH];

			// Iterate over all variables of the same type
			for (j = 0; j < field->vars->size; j++) {
				AST_UndefVar *var = field->vars->elements[j];
				
				// Field variable is scalar
				if (var->type == SCALAR) {
					if(*value_list_remainder <= 0) {
						break;
					}

					// Since the are remaining elements in the value list ensure the types are compatible
					if(field->type != value_list->elements[value_list->size - *value_list_remainder]->type) {
						sprintf(buffer, "<SEM> Value list for initialization of scalar variable '%s' has a value of an incompatible type", var->id);
						yyerror(buffer);
						return 1;
					}

					(*value_list_remainder)--;
				}
				// Field variable is array
				else if (var->type == ARRAY) {
					int size, k;
					
					// Calculate total number of array elements
					for (size = 1, k = 0; k < var->dims->size; k++) {
						size *= var->dims->elements[k];
					}
					size = (size <= *value_list_remainder) ? size : *value_list_remainder;

					// Check for type compatibility over all matching initialization values
					for (k = 0; k < value_list->size - size; k++) {
						if (field->type != value_list->elements[k]->type) {
							sprintf(buffer, "<SEM> Value list for initialization of array variable '%s' contains a value of incompatible type", var->id);
							yyerror(buffer);
							return 1;
						}
					}
					*value_list_remainder -= size;
				}
				// Field variable is list
				else {
					// There shouldn't be remaining initialization values matching a list variable
					if(*value_list_remainder > 0) {
						sprintf(buffer, "<SEM> List variable '%s' cannot be initialized in the data section", var->id);
						yyerror(buffer);
						return 1;
					}
				}
			}
		}
		// This field is also a record so call the same function recursively
		else {
			// Iterate over all variables of the same record datatype
			for (j = 0; j < field->vars->size; j++) {
				return recursive_SEM_check_REC_compatible_initialization(field->fields, field->size, value_list, value_list_remainder);
			}
		}
	}

	return 0;
}

int SEM_check_compatible_initialization(decl_t *decl, AST_Values *value_list)
{
	char buffer[MAX_STRING_LENGTH];

	// Raise an exception when the variable is a list since it cannot be initialized in the data section
	if (decl->variable->type == LIST) {
		sprintf(buffer, "<SEM> List variable '%s' cannot be initialized in the data section", decl->variable->id);
		yyerror(buffer);
		return 1;
	}

	// Variable is a scalar
	if (decl->variable->type == SCALAR) {
		// Scalar variable has a complex datatype (record), so check its fields recursively
		if (decl->datatype->type == REC) {
			int value_list_remainder = value_list->size;
			// TODO: Check if there are remaining elements after function call
			return recursive_SEM_check_REC_compatible_initialization(decl->datatype->fields->elements, decl->datatype->fields->size, value_list, &value_list_remainder);
		}
		// Scalar variable has a simple datatype hence the value list must have one element
		else if (value_list->size != 1) {
			sprintf(buffer, "<SEM> Value list for initialization of scalar variable '%s' hasn't exactly one value of the same type", decl->variable->id);
			yyerror(buffer);
			return 1;
		}
		// Scalar variable has a simple datatype but the value list contains one element of incompatible type
		else if (decl->datatype->type != value_list->elements[0]->type) {
			sprintf(buffer, "<SEM> Value list for initialization of scalar variable '%s' has a value of an incompatible type", decl->variable->id);
			yyerror(buffer);
			return 1;
		}
	}

	// Variable is a array
	if (decl->variable->type == ARRAY) {
		int size, i;
		
		// Calculate the total number of array elements
		for (size = 1, i = 0; i < decl->variable->dims->size; i++) {
			size *= decl->variable->dims->elements[i];
		}

		// Array variable has a complex datatype (record), so check its fields recursively
		if (decl->datatype->type == REC) {
			int value_list_remainder = value_list->size;
			// Check as many times as the elements of the array
			for (i = 0; i < size; i++) {
				// TODO: Check if there are remaining elements after function call
				return recursive_SEM_check_REC_compatible_initialization(decl->datatype->fields->elements, decl->datatype->fields->size, value_list, &value_list_remainder);
			}
		}
		// Array variable has a simple datatype
		else {
			// There shouldn't be any remaining initialization values 
			if (size < value_list->size) {
				sprintf(buffer, "<SEM> Value list for initialization of array variable '%s' has remaining unmatched values", decl->variable->id);
				yyerror(buffer);
				return 1;
			}

			// All values in the value list matching the array elements should be of the same type
			for (i = 0; i < value_list->size; i++) {
				if (decl->datatype->type != value_list->elements[i]->type) {
					sprintf(buffer, "<SEM> Value list for initialization of array variable '%s' contains a value of incompatible type", decl->variable->id);
					yyerror(buffer);
					return 1;
				}
			}
		}
	}

	return 0;
}