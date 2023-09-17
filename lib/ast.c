/****************************************************/
/************** AST IMPLEMENTATION ******************/
/****************************************************/
#include "ast.h"
#include "semantic.h"
#include "symbol_table.h"
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

/***** Globals *****/

int statement_counter = 0;

// Global array with all references to a label
label_uses_t label_uses = { .size = 0, .elements = NULL };


// Malloc with safety
void *safe_malloc(size_t size)
{
	void *ptr = malloc(size);
	if (ptr == NULL) {
		perror("Internal error.");
		exit(EXIT_FAILURE);
	}
	return ptr;
}

// Malloc with safety
void *safe_realloc(void *ptr, size_t size)
{
	void *new_ptr = realloc(ptr,size);
	if (new_ptr == NULL) {
		perror("Internal error.");
		exit(EXIT_FAILURE);
	}
	return new_ptr;
}

/****************************************************/
/********************* FUNCTIONS ********************/
/****************************************************/


/****** Functions for Declarations ******/

// Create ast node for integer constant
AST_Constant *ast_get_ICONST(int val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->intval = val;
	ret_val->type = INT;

	return ret_val;
}

// Create ast node for real constant
AST_Constant *ast_get_RCONST(double val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->rval = val;
	ret_val->type = REAL;

	return ret_val;
}

// Create ast node for character constant
AST_Constant *ast_get_CCONST(char val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->charval = val;
	ret_val->type = CHAR;

	return ret_val;
}

// Create ast node for logical constant
AST_Constant *ast_get_LCONST(bool val)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->lval = val;
	ret_val->type = LOG;

	return ret_val;
}

// Create ast node for logical constant
AST_Constant *ast_get_CMPLX(double re, AST_Sign im_sign, double im)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->cmplxval.re = re;
	if (im_sign != NONE) {
		ret_val->cmplxval.im = im * im_sign;
	}
	else {
		ret_val->cmplxval.im = im;
	}
	ret_val->type = CMPLX;

	return ret_val;
}

/****** Functions for Sign ******/

// Create ast node for sign
AST_Sign ast_get_sign(AST_Sign sign)
{
	return sign;
}

// Create ast node for constant value
AST_Constant *ast_get_value(AST_Sign sign, AST_Constant *constant)
{
	// Check if the constant and the sign are valid
	SEM_signable_constant(sign, constant);
	
	// Update the constant struct if the sign is minus
	if (sign == MINUS) {
		if (constant->type == INT) {
			constant->intval = -constant->intval;
		}
		else if (constant->type == REAL) {
			constant->rval = -constant->rval;
		}
		else {
			constant->cmplxval.re = -constant->cmplxval.re;
			constant->cmplxval.im = -constant->cmplxval.im;
		}
	}

	return constant;
}

// Create ast node for string value
AST_Constant *ast_get_string(char *strval)
{
	AST_Constant *ret_val = safe_malloc(sizeof(AST_Constant));
	ret_val->strval = strval;
	ret_val->type = STR;
	
	return ret_val;
}

AST_Values *ast_insert_value_to_values(AST_Values *values, AST_Constant *value)
{
	AST_Values *new_values;

	if (values == NULL) {
		// Case of the first value
		new_values = safe_malloc(sizeof(AST_Values));
		new_values->size = 1;
		new_values->elements = NULL;
	}
	else {
		// Case of the other values
		new_values = values;
		new_values->size++;
	}

	// Extend the array with the new element
	new_values->elements = safe_realloc(new_values->elements, new_values->size * sizeof(AST_Constant *));
	new_values->elements[new_values->size - 1] = value;

	return new_values;
}

AST_Vals *ast_insert_val_to_vals(AST_Vals *vals, char *id, AST_Values *value_list)
{
	AST_Vals *new_vals;

	// Use hashtbl utilities to check if the variable has been declared
	// SEM_declaration_check(char *id);

	if(vals == NULL) {
		// Case of the first value
		new_vals = safe_malloc(sizeof(AST_Vals));
		new_vals->size = 1;
		new_vals->elements = NULL;
	}
	else {
		// Case of the other values
		new_vals = vals;
		new_vals->size++;
	}
	
	// Extend pointer array
	new_vals->elements = safe_realloc(new_vals->elements, new_vals->size * sizeof(init_val_t *));
	// Allocate memory for InitVal struct
	new_vals->elements[new_vals->size - 1] = safe_malloc(sizeof(init_val_t));
	
	// Update the struct fields
	new_vals->elements[new_vals->size - 1]->id = id;
	new_vals->elements[new_vals->size - 1]->value_list = value_list;

	return new_vals;
}

// Create dim struct for literal value of dim
AST_Dim *ast_get_dim_literal(int val)
{
	AST_Dim *dim = safe_malloc(sizeof(AST_Dim));
	dim->type = DIM_LITERAL;
	dim->val = val;
	printf("<create:%p>\n", dim);
	return dim;
}

// Create dim struct for variable dim
AST_Dim *ast_get_dim_variable(char *id)
{
	AST_Dim *dim = safe_malloc(sizeof(AST_Dim));
	dim->type = DIM_VARIABLE;
	dim->id = id;

	return dim;
}

// Create or append to dimentions
AST_Dims *ast_insert_dim_to_dims(AST_Dims *dims, AST_Dim *dim)
{
	AST_Dims *new_dims;

	if (dims == NULL) {
		// Case of the first value
		new_dims = safe_malloc(sizeof(AST_Dims));
		new_dims->size = 1;
		new_dims->elements = NULL;
	}
	else {
		// Case of the other values
		new_dims = dims;
		new_dims->size++;
	}

	// Extend the array with the new element
	new_dims->elements = safe_realloc(new_dims->elements, new_dims->size * sizeof(AST_Dim *));
	new_dims->elements[new_dims->size - 1] = dim;

	return new_dims;
}

// Create AST node for an undefined variable
AST_UndefVar *ast_get_undef_var(AST_UndefVar_Type type, char *id, AST_Dims *dims, AST_UndefVar *nested_undef_var)
{
	AST_UndefVar *ret_val;
	if (type == LIST) {
		ret_val = nested_undef_var;
		assert(id == NULL);
		ret_val->list_depth++;
		SEM_check_list_depth(ret_val);
	}
	else {
		ret_val = safe_malloc(sizeof(AST_UndefVar));
		assert(nested_undef_var == NULL);
		ret_val->id = id;
		ret_val->list_depth = 0;
		ret_val->type = type;
		ret_val->dims = dims;
	}


	return ret_val;
}

// Create AST node for Vars if it didn't exist or add an UndefVar to it
AST_Vars *ast_insert_var_to_vars(AST_Vars *vars, AST_UndefVar *var)
{
	AST_Vars *new_vars;

	if (vars == NULL) {
		// Case of the first value
		new_vars = safe_malloc(sizeof(AST_Vars));
		new_vars->size = 1;
		new_vars->elements = NULL;
	}
	else {
		// Case of the other values
		new_vars = vars;
		new_vars->size++;
	}

	// Extend the array with the new element
	new_vars->elements = safe_realloc(new_vars->elements, new_vars->size * sizeof(AST_UndefVar *));
	new_vars->elements[new_vars->size - 1] = var;

	return new_vars;
}

// Create AST node for fields if it didn't exist or add a field to it
AST_Fields *ast_insert_field_to_fields(AST_Fields *fields, AST_Field *field)
{
	AST_Fields *new_fields;

	if (fields == NULL) {
		// Case of the first value
		new_fields = safe_malloc(sizeof(AST_Fields));
		new_fields->size = 1;
		new_fields->elements = NULL;
	}
	else {
		// Case of the other values
		new_fields = fields;
		new_fields->size++;
	}

	// Extend the array with the new element
	new_fields->elements = safe_realloc(new_fields->elements, new_fields->size * sizeof(AST_Field *));
	new_fields->elements[new_fields->size - 1] = field;

	return new_fields;
}

// Get an ast node for a record's field
AST_Field *ast_get_field(type_t type, AST_Vars *vars, AST_Fields *fields)
{
	AST_Field *ret_val = safe_malloc(sizeof(AST_Field));
	ret_val->type = type;
	ret_val->vars = vars;
	
	// if field is a record it contains subfields that reside in fields
	if (type == REC && fields != NULL) {
		ret_val->fields = fields;
	}
	else {
		ret_val->fields = NULL;
	}

	return ret_val;
}

// Given an AST_Vars struct break it in 
// several devl_t structs, one for each id
AST_Decls *ast_insert_decl_to_decls(AST_Decls *old_decls, type_t type, AST_Fields *fields, AST_Vars *vars)
{
	AST_Decls *new_decls;
	char *id;
	
	if (old_decls == NULL) {
		new_decls = safe_malloc(sizeof(AST_Decls));
		new_decls->size = 0;
		new_decls->declarations = NULL;
	}
	else{
		new_decls = old_decls;
	}

	// Extend the declarations by the number of ids in vars
	int old_size = new_decls->size;
	new_decls->size += vars->size;
	
	new_decls->declarations = safe_realloc(new_decls->declarations, new_decls->size * sizeof(decl_t *));
	
	for (int i = 0; i < vars->size; i++) {
		SEM_check_duplicate_variable_name(vars->elements[i]->id);
		new_decls->declarations[old_size + i] = safe_malloc(sizeof(decl_t));
		new_decls->declarations[old_size + i]->is_parameter = false;
		new_decls->declarations[old_size + i]->datatype = safe_malloc(sizeof(AST_GeneralType));
		new_decls->declarations[old_size + i]->datatype->type = type;
		new_decls->declarations[old_size + i]->datatype->fields = fields;
		new_decls->declarations[old_size + i]->variable = vars->elements[i];
		
		AST_Dims *dims = new_decls->declarations[old_size + i]->variable->dims;
		if (dims != NULL) {
			SEM_check_declared_dims(dims, false);
		}

		// Insert in symbol table
		id = new_decls->declarations[old_size + i]->variable->id;
		stbl_insert_variable(id, new_decls->declarations[old_size + i]);
	}

	return new_decls;
}

// Insert initialization value of variable to its declaration struct
void ast_insert_init_in_decls(AST_Vals *vals)
{
	init_val_t *curr_val;
	AST_Values *curr_value_list;
	char *curr_id;
	decl_t *entry;
	int error;

	for(int i = 0; i < vals->size; i++) {
		curr_val = vals->elements[i];
		curr_id = curr_val->id;
		curr_value_list = curr_val->value_list;
		
		// Find decl of variable from symbol table
		entry = stbl_search_variable(curr_id);
		error = SEM_check_existing_variable(entry, curr_id);
		
		// Function not implemented
		if (!error)
			error =	SEM_check_compatible_initialization(entry, curr_value_list);
		if (!error)
			entry->initial_value = curr_value_list;
	}
}

/****** Functions for Statements ******/

// Insert label_use to label_uses array
void insert_label_use(int label, AST_Statement **statement_addr)
{
	label_uses.size++;
	label_uses.elements = safe_realloc(label_uses.elements, label_uses.size * sizeof(label_use_t *));
	label_uses.elements[label_uses.size-1] = safe_malloc(sizeof(label_use_t));
	label_uses.elements[label_uses.size-1]->min_scope = stbl_get_curr_scope();
	label_uses.elements[label_uses.size-1]->label = label;
	// Store address of pointer
	label_uses.elements[label_uses.size-1]->statement_addr = (AST_Statement**)statement_addr;
}

// When a label_use is matched to a label, delete it
void remove_label_use(int index)
{
	assert(index < label_uses.size);

	label_uses.size--;
	free(label_uses.elements[index]);
	label_uses.elements[index] = label_uses.elements[label_uses.size];
	
	if (label_uses.size > 0) {
		label_uses.elements = safe_realloc(label_uses.elements, label_uses.size * sizeof(label_use_t *));
	}
	else {
		free(label_uses.elements);
		label_uses.elements = NULL;
	}
}

// When closing a scope, match label uses that were created
// in that scope to labels. Otherwise reduce the min scope
// variable so they cannot be matched with labels in other 
// cousin scopes [(children of)^n (parent of)^n-1 parent]
void match_labels_to_label_uses()
{
	int curr_scope = stbl_get_curr_scope();

	for (int i = 0; i < label_uses.size; i++) {
		label_use_t *label_use = label_uses.elements[i];
		if (label_use->min_scope == curr_scope) {
			label_t *label = stbl_search_label_in_current_scope(label_use->label);

			if (label == NULL) {
				label_use->min_scope--;
			}
			else {
				*(label_use->statement_addr) = label->next_statement;
				remove_label_use(i);
			}
		}
	}
}



// Create struct for a goto with a single label
AST_Goto *ast_get_independent_goto(int label)
{
	AST_Goto *goto_statement = safe_malloc(sizeof(AST_Goto));
	goto_statement->variable = NULL;

	// Insert the goto statement as a label_use with the statement to be filled later
	insert_label_use(label, &(goto_statement->statement));
	return goto_statement;
}

// AST_Goto *ast_get_computed_goto(char *id, int *labels)
// {
// 	AST_Goto *goto_statement = safe_malloc(sizeof(AST_Goto));
// 	goto_statement->variable = stbl_search_variable();
// 	return NULL;
// }


// Create the struct of statement
AST_Statement *ast_get_statement(statement_type_t type, void *ptr)
{
	AST_Statement *stmt = safe_malloc(sizeof(AST_Statement));
	stmt->statement_id = statement_counter++;
	stmt->type = type;
	stmt->next = NULL;


	if (type == SIMPLE) {
		stmt->simple = ptr;
	}
	else {
		stmt->compound = ptr;
	}

	return stmt;
}

// Create AST node for Statements if it didn't exist or add a Statement to it
AST_Statements *ast_insert_statement_to_statements(AST_Statements *stmts, AST_Statement *stmt)
{
	AST_Statements *new_stmts;

	if (stmts == NULL) {
		// Case of the first value
		new_stmts = safe_malloc(sizeof(AST_Statements));
		new_stmts->head = stmt;
		new_stmts->tail = stmt;
	}
	else {
		// Case of the other values
		new_stmts = stmts;

		new_stmts->tail->next = stmt;
		new_stmts->tail = stmt;
	}

	return new_stmts;
}

// Create the struct of a simple statement
AST_SimpleStatement *ast_get_simple_statement(simple_statement_type_t type, void *ptr)
{
	AST_SimpleStatement *stmt = safe_malloc(sizeof(AST_SimpleStatement));
	stmt->type = type;

	switch (type)
	{
		case ASSIGNMENT:
			stmt->assignment = (AST_Assignment *) ptr;
		case GOTO:
			stmt->goto_statement = (AST_Goto *) ptr;
			break;

		default:
			break;
	}

	return stmt;
}

// Get a variable struct
AST_Variable *ast_get_variable_id(char *id)
{	
	printf("Get variable id %s\n", id);
	AST_Variable *new_variable = safe_malloc(sizeof(AST_Variable));
	new_variable->type = V_ID;
	new_variable->datatype = NULL;
	new_variable->id = id;
	
	return new_variable;
}

// Search for an id that is a field in a record and return the field and the position/index
// of the id in the AST_Vars array of the field
AST_Field* ast_search_field_in_record(AST_GeneralType *datatype, char *field_id, int *position_in_vars)
{
	assert(datatype->fields != NULL);

	// Search in all fields of RECORD
	for (int i = 0; i < datatype->fields->size; i++) {
		AST_Field *field = datatype->fields->elements[i];

		// Search in all vars of each field
		for (int j = 0; j < field->vars->size; j++) {
			AST_UndefVar *var = field->vars->elements[j];

			// Compare strings
			if (strcmp(var->id, field_id) == 0) {
				// Return the field and the index of the var
				*position_in_vars = j;
				return field;
			}
		}
	}

	return NULL;
}

// Transforms variables from id to decl
int ast_variable_id2decl(AST_Variable *variable)
{	
	if (variable->type != V_ID) {
		return 1;
	}

	decl_t *decl = stbl_search_variable(variable->id);

	if (SEM_check_existing_variable(decl, variable->id)) {
		return 1;
	}

	variable->type = V_DECL;
	variable->decl = decl;
	variable->datatype = decl->datatype;

	return 0;
}

// Create a record access node
AST_Variable *ast_get_variable_rec_access(AST_Variable *rec, char *field_id)
{
	int position_in_vars;
	
	// If the rec is in ID form change it to decl
	if (rec->type == V_ID)
		ast_variable_id2decl(rec);

	// Make sure no rec->type related bug exists here
	assert(rec->type != V_FUNC_CALL && rec->type != V_ID);
	
	SEM_check_decl_datatype_simple(rec->decl->datatype, REC, rec->id);
	AST_Field* field = ast_search_field_in_record(rec->decl->datatype, field_id, &position_in_vars);
	
	if (SEM_check_existing_record_field(field, rec->id, field_id)) {
		return NULL;
	}

	// Create new node for new_variable
	AST_Variable *new_variable = safe_malloc(sizeof(AST_Variable));
	
	// Fill the new_variable contents
	new_variable->type = V_REC_ACCESS;
	new_variable->record = rec;

	// Copy datatype of variable
	AST_GeneralType *new_datatype = safe_malloc(sizeof(AST_GeneralType));
	new_datatype->type = field->type;
	new_datatype->fields = field->fields;

	new_variable->datatype = new_datatype;

	// Store the variable field
	new_variable->field_var = field->vars->elements[position_in_vars];

	return new_variable;
}

/* Functions for creating expression nodes */

// Create an expression node that is a variable
AST_Expression *ast_get_expression_var(AST_Variable *variable)
{
	if (variable->type == V_ID)
		ast_variable_id2decl(variable);

	AST_Expression *new_expr = safe_malloc(sizeof(AST_Expression));
	new_expr->expr_type = EXPR_VARIABLE;
	new_expr->variable = variable;	
	new_expr->datatype = variable->datatype;
	
	switch (variable->type)
	{
	case V_DECL:
		new_expr->list_depth = variable->decl->variable->list_depth;
		printf("Create expression from variable->decl: ");
		ast_print_variable(variable->decl->variable, " ");
		break;
	case V_REC_ACCESS:
		new_expr->list_depth = variable->field_var->list_depth;
		break;
	case V_FUNC_CALL:
		break;
	case V_ARRAY_ACCESS:
		break;
	case V_LISTFUNC:
		break;
	default:
		break;
	}

	return new_expr;
}

// Get a constant struct
AST_Expression *ast_get_constant_expr(AST_Constant *constant)
{
	AST_Expression *new_expr = safe_malloc(sizeof(AST_Expression));
	new_expr->expr_type = EXPR_CONSTANT;
	new_expr->constant = constant;
	new_expr->datatype =safe_malloc(sizeof(AST_GeneralType));
	new_expr->datatype->type = constant->type;
	new_expr->datatype->fields = NULL;

	printf("Create expression %p from constant with type %d\n", new_expr, constant->type);
	return new_expr;
}

// Creates an AST node for a unary expression
AST_Expression *ast_get_expression_unary(AST_Expression *child, unary_op_t op_type)
{
	AST_Expression *new_expr = safe_malloc(sizeof(AST_Expression));
	new_expr->expr_type = EXPR_UNARY;
	new_expr->unary.op = op_type;
	new_expr->unary.child = child;
	
	switch (op_type)
	{
	case U_PLUS:
	case U_MINUS:
		if (SEM_check_expr_datatype(child, O_INTEGER | O_REAL | O_COMPLEX))
			return NULL;
		new_expr->datatype = child->datatype;
		break;
	case U_NOT:
		if (SEM_check_expr_datatype(child, O_LOGICAL))
			return NULL;
		new_expr->datatype = child->datatype;
		break;
	case U_LENGTH:
		if (SEM_check_list(child))
			return NULL;
		new_expr->datatype = safe_malloc(sizeof(AST_GeneralType));
		new_expr->datatype->type = INT;
		new_expr->datatype->fields = NULL;
		break;
	case U_NEW:
		new_expr->datatype = child->datatype;
		new_expr->list_depth = child->list_depth + 1;
		break;
	default:
		break;
	}

	return new_expr;
}

// Create unary expression for notop
AST_Expression *ast_get_expression_unary_notop(AST_Expression *child)
{
	return ast_get_expression_unary(child, U_NOT);
}

// Create unary expression for addop
AST_Expression *ast_get_expression_unary_addop(AST_Expression *child, AST_Sign sign)
{
	if (sign == 1)
		return ast_get_expression_unary(child, U_PLUS);
	else
		return ast_get_expression_unary(child, U_MINUS);
}

// Create unary expression for addop
AST_Expression *ast_get_expression_unary_length(AST_Expression *child)
{
	return ast_get_expression_unary(child, U_LENGTH);
}

// Create unary expression for addop
AST_Expression *ast_get_expression_unary_new(AST_Expression *child)
{
	return ast_get_expression_unary(child, U_NEW);
}

// Creates an AST node for a binary expression
AST_Expression *ast_get_expression_binary(binary_op_t operation, AST_Expression *child1, AST_Expression *child2)
{
	AST_Expression *new_expr = safe_malloc(sizeof(AST_Expression));
	new_expr->expr_type = EXPR_BINARY;
	new_expr->binary.child1 = child1;
	new_expr->binary.child2 = child2;
	new_expr->binary.op = operation;

	new_expr->datatype = safe_malloc(sizeof(AST_GeneralType));
	new_expr->datatype->fields = NULL;
	
	switch (operation)
	{
	case B_OR:
	case B_AND:
		new_expr->datatype->type = LOG;
		if (SEM_check_expr_datatype(child1, O_LOGICAL | O_INTEGER | O_REAL))
			return NULL;
		if (SEM_check_expr_datatype(child2, O_LOGICAL | O_INTEGER | O_REAL))
			return NULL;
		break;
	case B_PLUS:
	case B_MINUS:
	case B_MUL:
	case B_DIV:
		// Check valid datatypes of children
		SEM_check_expr_datatype(child1, O_INTEGER | O_REAL | O_COMPLEX);
		SEM_check_expr_datatype(child2, O_INTEGER | O_REAL | O_COMPLEX);

		// Find final expr datatype
		if (child1->datatype->type == CMPLX || child2->datatype->type == CMPLX) {
			new_expr->datatype->type = CMPLX;
		}
		else if (child1->datatype->type == REAL || child2->datatype->type == REAL){
			new_expr->datatype->type = REAL;
		}
		else {
			new_expr->datatype->type = INT;
		}
	case B_POWER:
		// Check valid datatypes of children
		SEM_check_expr_datatype(child2, O_INTEGER | O_REAL);
		if(child2->datatype->type == INT) {
			SEM_check_expr_datatype(child1, O_INTEGER | O_REAL | O_COMPLEX);
		}
		else {
			SEM_check_expr_datatype(child1, O_INTEGER | O_REAL);
		}
		
		// Find final expr datatype
		if(child1->datatype->type == CMPLX) {
			new_expr->datatype->type = CMPLX;
		}
		else if (child1->datatype->type == REAL || child2->datatype->type == REAL){
			new_expr->datatype->type = REAL;
		}
		else {
			new_expr->datatype->type = INT;
		}
	default:
		break;
	}

	return new_expr;
}

// Wrapper of ast_get_expression_binary for orop
AST_Expression *ast_get_expression_binary_orop(AST_Expression *child1, AST_Expression *child2)
{
	return ast_get_expression_binary(B_OR, child1, child2);
}

// Wrapper of ast_get_expression_binary for andop
AST_Expression *ast_get_expression_binary_andop(AST_Expression *child1, AST_Expression *child2)
{	
	return ast_get_expression_binary(B_AND, child1, child2);
}

// Wrapper of ast_get_expression_binary for relop
AST_Expression *ast_get_expression_binary_relop(AST_Relop op, AST_Expression *child1, AST_Expression *child2)
{
	return ast_get_expression_binary(B_GT + op, child1, child2);
}

// Wrapper of ast_get_expression_binary for addop
AST_Expression *ast_get_expression_binary_addop(AST_Sign sign, AST_Expression *child1, AST_Expression *child2)
{
	if (sign == 1)
		return ast_get_expression_binary(B_PLUS, child1, child2);
	else
		return ast_get_expression_binary(B_MINUS, child1, child2);
}

// Wrapper of ast_get_expression_binary for mulop
AST_Expression *ast_get_expression_binary_mulop(AST_Expression *child1, AST_Expression *child2)
{
	return ast_get_expression_binary(B_MUL, child1, child2);
}

// Wrapper of ast_get_expression_binary for divop
AST_Expression *ast_get_expression_binary_divop(AST_Expression *child1, AST_Expression *child2)
{
	return ast_get_expression_binary(B_DIV, child1, child2);
}

// Wrapper of ast_get_expression_binary for pwrop
AST_Expression *ast_get_expression_binary_pwrop(AST_Expression *child1, AST_Expression *child2)
{
	return ast_get_expression_binary(B_POWER, child1, child2);
}

// Create unary expression for addop
AST_Expression *ast_get_expression_binary_cmplx(AST_Expression *child1, AST_Expression *child2)
{
	return ast_get_expression_binary(B_CMPLX, child1, child2);
}



/* Functions for assignents */

// Create an assignment to string
AST_Assignment *ast_get_assignment_expression(AST_Variable *variable, AST_Expression *expression)
{
	if (variable->type == V_ID)
		ast_variable_id2decl(variable);

	if (SEM_typecheck_variable(variable, expression->datatype->type, "Left hand of assignment")) {
		return NULL;
	}
	printf("Create new assignment node with variable and expression\n");

	AST_Assignment *asmt = safe_malloc(sizeof(AST_Assignment));

	asmt->variable = variable;
	asmt->type = AS_EXPRESSION;
	asmt->expression = expression;

	return asmt;
}

// Create an assignment to string
AST_Assignment *ast_get_assignment_string(AST_Variable *variable, char *string)
{
	if (variable->type == V_ID)
		ast_variable_id2decl(variable);
		
	if (SEM_typecheck_variable(variable, STR, "Left hand of assignment")) {
		return NULL;
	}

	AST_Assignment *asmt = safe_malloc(sizeof(AST_Assignment));
	
	asmt->variable = variable;
	asmt->type = AS_STRING;
	asmt->string = string;

	return asmt;
}


/****** Functions for Program & Subprograms ******/

// Given an AST_Vars struct break it in 
// several decl_t structs, one for each id
AST_Params *ast_insert_param_to_params(AST_Params *old_params, type_t type, AST_Vars *vars)
{
	AST_Params *new_params;

	if (old_params == NULL) {
		new_params = safe_malloc(sizeof(AST_Params));
		new_params->size = 0;
		new_params->elements = NULL;
	}
	else{
		new_params = old_params;
	}

	// Extend the parameters by the number of ids in vars
	int old_size = new_params->size;
	
	new_params->size += vars->size;
	new_params->elements = safe_realloc(new_params->elements, new_params->size * sizeof(decl_t *));
	
	for (int i = 0; i < vars->size; i++) {
		SEM_check_duplicate_variable_name(vars->elements[i]->id);
		new_params->elements[old_size + i] = safe_malloc(sizeof(decl_t));
		new_params->elements[old_size + i]->is_parameter = true;
		new_params->elements[old_size + i]->datatype = safe_malloc(sizeof(AST_GeneralType));
		new_params->elements[old_size + i]->datatype->type = type;
		new_params->elements[old_size + i]->datatype->fields = NULL; // No field as parameter
		new_params->elements[old_size + i]->variable = vars->elements[vars->size - i - 1];
	}

	return new_params;
}

// Ensure that the dimensions of any parameter arrays have been declared
void ast_check_params(AST_Params *params)
{
	decl_t *tmp;
	char *id;

	// Reverse the parameters for them to be in right order
	for (int i = 0; i < params->size / 2; i++) {
		tmp = params->elements[i];
		params->elements[i] = params->elements[params->size - i - 1];
		params->elements[params->size - i - 1] = tmp;
	}

	for (int i = 0; i < params->size; i++) {
		// Ensure that existing dimensions are declared
		AST_Dims *dims = params->elements[i]->variable->dims;
		if (dims != NULL) {
			SEM_check_declared_dims(dims, true);
		}
		
		// Insert the parameter ids in the symbol table
		id = params->elements[i]->variable->id;
		stbl_insert_variable(id, params->elements[i]);
	}
}

// Create a struct for the header of a function
AST_Header *ast_get_header(subprogram_type_t subprogram_type, type_t type, bool is_list, char *id, AST_Params *params)
{
	// Semantic checks
	if (subprogram_type == FUNCTION) {
		SEM_check_existing_arguments(params, id);
	}
	SEM_check_duplicate_subprogram_name(id);
	
	// Struct creation
	AST_Header *header = safe_malloc(sizeof(AST_Header));
	header->subprogram_type = subprogram_type;
	header->id = id;
	header->params = params;

	if (subprogram_type == FUNCTION) {
		header->returns_list = is_list;
		header->ret_type = type;
	}

	return header;
}

// Create a subprogram structure
AST_Subprogram *ast_get_subprogram(AST_Header *header, AST_Body *body)
{
	AST_Subprogram *subprogram = safe_malloc(sizeof(AST_Subprogram));
	subprogram->header = header;
	subprogram->body = body;
	return subprogram;
}

// Create or append to an AST_Subprograms struct with a new AST_Subprogram element
AST_Subprograms *ast_insert_subprogram_to_subprograms(AST_Subprograms *subprograms, AST_Subprogram *subprogram)
{
	AST_Subprograms *new_subprograms;

	if (subprograms == NULL) {
		// Case of the first value
		new_subprograms = safe_malloc(sizeof(AST_Subprogram));
		new_subprograms->size = 1;
		new_subprograms->elements = NULL;
	}
	else {
		// Case of the other values
		new_subprograms = subprograms;
		new_subprograms->size++;
	}

	// Extend the array with the new element
	new_subprograms->elements = safe_realloc(new_subprograms->elements, new_subprograms->size * sizeof(AST_Subprogram *));
	new_subprograms->elements[new_subprograms->size - 1] = subprogram;

	char *id = new_subprograms->elements[new_subprograms->size - 1]->header->id;
	stbl_insert_subprogram(id, new_subprograms->elements[new_subprograms->size - 1]);

	return new_subprograms;
}

// Create a body structure
AST_Body *ast_get_body(AST_Decls *decls, AST_Statements *statements)
{
	AST_Body *body = safe_malloc(sizeof(AST_Body));
	body->declarations = decls;
	body->statements = statements;
	return body;
}

// Create the high level structure of a program
AST_Program *ast_get_program(AST_Body *main, AST_Subprograms *subprograms)
{
	AST_Program *prog = safe_malloc(sizeof(AST_Program));
	prog->main = main;
	prog->subprograms = subprograms;
	return prog;
}



/****************************************************/
/******************* DEBUG & PRINTS *****************/
/****************************************************/

// Returns a string with one more tab than the input string
char *more_tabs(char *tabs)
{
	size_t size = strlen(tabs);
	char *new_tabs = safe_malloc(size+2);
	if (size!=0)
		strcpy(new_tabs, tabs);
	new_tabs[size] = '\t';
	new_tabs[size+1] = '\0';

	return new_tabs;
}

char *functype_str[] = {"SUBROUTINE", "FUNCTION"};
char *type_str[] = {"INTEGER", "LOGICAL", "REAL", "CHARACTER", "STRING", "COMPLEX", "RECORD"};

// Print Info about the header of a unit
void ast_print_header(AST_Header *header)
{
	printf("%s ", functype_str[header->subprogram_type]); 
	if (header->subprogram_type == FUNCTION) {
		printf("%s ", header->returns_list ? "LIST" : type_str[header->ret_type]);
	}
	printf("%s\n", header->id);

	if (header->params != NULL) {
		
		// Iterate over the parameters
		for (int i = 0; i < header->params->size; i++) {
			printf("\t%s %s ",
			type_str[header->params->elements[i]->datatype->type],
			header->params->elements[i]->variable->id);
			
			if (header->params->elements[i]->variable->list_depth)
				printf("[list depth: %d] ", header->params->elements[i]->variable->list_depth);

			AST_Dims* dims = header->params->elements[i]->variable->dims;
			if (dims != NULL) {
				printf("(");
				for (int j = 0; j < dims->size - 1; j++) {
					if (dims->elements[j]->type == DIM_LITERAL) {
						printf("%d, ", dims->elements[j]->val);
					}
					else {
						printf("%s, ", ((decl_t *)dims->elements[j]->decl)->variable->id);
					}
				}
				if (dims->elements[dims->size-1]->type == DIM_LITERAL) {
					printf("%d)", dims->elements[dims->size-1]->val);
				}
				else {
					printf("%s)", ((decl_t *)dims->elements[dims->size-1]->decl)->variable->id);
				}
			}
			printf("\n");
		}
	}

	return;
}

// Dummy for printing fields
void ast_print_fields(AST_Fields *fields, char *tabs)
{
	for (int i = 0; i < fields->size; i++) {
		AST_Field *field = fields->elements[i];
		printf("%s%s", tabs, type_str[field->type]);
		
		if (field->type != REC) {

			AST_Vars *vars = field->vars;
			for (int j = 0; j < vars->size; j++) {
				AST_UndefVar *uvar = vars->elements[j];
				printf(" <LDEPTH-%d> ", uvar->list_depth);
				
				if (uvar->type == ARRAY) {
					printf("<dims:");
					for (int d = 0; d < uvar->dims->size; d++) {
						if (uvar->dims->elements[d]->type == DIM_LITERAL) {
							printf("%d, ", uvar->dims->elements[d]->val);
						}
						else {
							printf("%s, ", uvar->dims->elements[d]->decl->variable->id);
						}
					}
					printf(">");
				}

				printf("%s", uvar->id);
			}

			putchar('\n');
		}
		else {
			char *mtabs = more_tabs(tabs);
			ast_print_fields(field->fields, mtabs);
		}
	}
	printf("%s", tabs);
	return;
}

// Prints type without \n unless it is record
// for records it prints its fields one line each
void ast_print_datatype(AST_GeneralType *datatype, char *tabs)
{
	char *mtabs = more_tabs(tabs);
	
	printf("%s%s", tabs, type_str[datatype->type]);
	if (datatype->type == REC) {
		printf("\n");
		ast_print_fields(datatype->fields, mtabs);
	}
}

void ast_print_variable(AST_UndefVar *variable, char *tabs)
{
	printf("%s", tabs);
	if (variable->list_depth) {
		printf("list(depth:%d) ", variable->list_depth);
	}

	printf("%s", variable->id);

	if (variable->type == ARRAY) {
		printf("(");
		for (int j = 0; j < variable->dims->size - 1; j++) {
			if (variable->dims->elements[j]->type == DIM_LITERAL) {
				printf("%d, ", variable->dims->elements[j]->val);
			}
			else {
				printf("%s, ", ((decl_t *)variable->dims->elements[j]->decl)->variable->id);
			}
		}
		if (variable->dims->elements[variable->dims->size-1]->type == DIM_LITERAL) {
			printf("%d)", variable->dims->elements[variable->dims->size-1]->val);
		}
		else {
			printf("%s)", ((decl_t *)variable->dims->elements[variable->dims->size-1]->decl)->variable->id);
		}
	}

	printf("\n");
}

void ast_print_variable_access(AST_Variable *variable, char *tabs) 
{	
	char *variable_type_str[] = {"ID", "DECL", "FUNC_CALL", "ARRAY_ACCESS", "REC_ACCESS", "LISTFUNC"};
	printf("%svariable type: %s\n", tabs, variable_type_str[variable->type]);

	switch(variable->type) {
		case V_DECL:
			ast_print_decl(variable->decl, tabs);
			break;
		case V_REC_ACCESS:
			printf("%s(printing record not supported yet)\n", tabs);
			break;
		default:
			break;
	}
}

void ast_print_constant(AST_Constant* constant, char *tabs)
{
	switch (constant->type)
	{
	case INT:
		printf("%sINT(%d)\n", tabs, constant->intval);
		break;
	case REAL:
		printf("%sREAL(%lf)\n", tabs, constant->rval);
		break;
	case LOG:
		printf("%sLOG(%d)\n", tabs, constant->lval);
		break;
	case CHAR:
		printf("%sCHAR(%c)\n", tabs, constant->charval);
		break;
	case CMPLX:
		printf("%sCMPLX(%lf, %lf)\n", tabs, constant->cmplxval.re, constant->cmplxval.im);
	default:
		break;
	}
}

void ast_print_initial_value(AST_Values *values, char *tabs)
{
	char *mtabs = more_tabs(tabs);
	for (int i = 0; i < values->size; i++) {
		ast_print_constant(values->elements[i], mtabs);
	}
	return;
}

void ast_print_decl(decl_t *decl, char *tabs)
{
	ast_print_datatype(decl->datatype, tabs);
	ast_print_variable(decl->variable, " ");
	if (decl->initial_value != NULL)
		ast_print_initial_value(decl->initial_value, tabs);
	printf("\n");
}

void ast_print_decls(AST_Decls *decls, char *tabs)
{
	if (decls)
		for (int i = 0; i < decls->size; i++) {
			ast_print_decl(decls->declarations[i], tabs);
		}
}

void ast_print_statements(AST_Statements *statements, char *tabs)
{
	AST_Statement *ptr;
	char *mtabs = more_tabs(tabs);
	
	for (ptr = statements->head; ptr != NULL; ptr = ptr->next) {
		printf("%s<stmt-%d>\n", tabs, ptr->statement_id);
		if (ptr->type == SIMPLE) {
			ast_print_simple_statement(ptr->simple, mtabs);
		}
		else if (ptr->type == COMPOUND) {
			ast_print_compound_statement(ptr->compound, mtabs);
		}
		printf("\n");
	}
}

void ast_print_simple_statement(AST_SimpleStatement *statement, char  *tabs)
{
	char *simple_stmt_type_str[] = {"ASSIGNMENT", "GOTO", "IF", "CALL_SUBROUTINE", "IO", "CONTINUE", "RETURN", "STOP"};
	char *mtabs = more_tabs(tabs);
	
	printf("%s%s statement\n", tabs, simple_stmt_type_str[statement->type]);

	switch (statement->type)
	{
	case GOTO:
		if (statement->goto_statement->variable == NULL) {
			printf("%sGOTO -> stmt %d\n", tabs, statement->goto_statement->statement->statement_id);
		}
		// else {
		// 	printf("%s GOTO depends on variable:\n", tabs);

		// 	ast_print_variable_access(statement->variable, mtabs);
		// 	printf("%s statement id list:", tabs);
		// 	for (AST_Statement *ptr; ptr != NULL; )

		// 	for 
		// }
		break;
	case ASSIGNMENT:
		printf("%sAssign to variable: \n", tabs);
		ast_print_variable_access(statement->assignment->variable, mtabs);
		
		if (statement->assignment->type == AS_STRING) {
			printf("%s The string \"%s\"", tabs, statement->assignment->string);
		}
		else if (statement->assignment->type == AS_EXPRESSION) {
			ast_print_expression(statement->assignment->expression, mtabs);
		}
		break;
	default:
		break;
	}
}

void ast_print_expression(AST_Expression *expression, char *tabs)
{
	char *unary_op_str[] =  {"PLUS", "MINUS", "NOT", "LENGTH", "NEW"};
	char *binary_op_str[] = {"PLUS","MINUS", "AND", "OR", "GT", "GE", "LT", "LE", "EQ", "NE", "MUL", "DIV", "POWER", "CMPLX"};
	char *mtabs = more_tabs(tabs);


	if (expression->expr_type == EXPR_UNARY) {
		printf("%sop: %s\n", tabs, unary_op_str[expression->unary.op]);
		printf("%schild: \n", tabs);
		ast_print_expression(expression->unary.child, mtabs);
	}
	else if (expression->expr_type == EXPR_BINARY) {
		printf("%sop: %s\n", tabs, binary_op_str[expression->binary.op]);
		printf("%schild1: \n", tabs);
		ast_print_expression(expression->binary.child1, mtabs);
		printf("%schild2: \n", tabs);
		ast_print_expression(expression->binary.child2, mtabs);
	}
	else if (expression->expr_type == EXPR_VARIABLE) {
		printf("%sExpression reduces to variable: \n", tabs);
		ast_print_variable_access(expression->variable, mtabs);
	}
	else if (expression->expr_type == EXPR_CONSTANT) {
		ast_print_constant(expression->constant, tabs);
	}
}


void ast_print_compound_statement(AST_CompoundStatement *statement, char  *tabs)
{
	return;
}



void ast_print_body(AST_Body *body, char *tabs)
{
	char *mtabs = more_tabs(tabs);

	printf("%sDeclarations:\n", tabs);
	ast_print_decls(body->declarations, mtabs);
	printf("%sStatements:\n", tabs);
	ast_print_statements(body->statements, mtabs);
	// printf("\"%s\"\n", mtabs);
	
	free(mtabs);
}

// Function for printing a subprogram ast
void ast_print_subprogram(AST_Subprogram *subgprogram)
{
	printf("+++++++++++++++++++++++++++++++++++++\n");
	ast_print_header(subgprogram->header);
	ast_print_body(subgprogram->body, "");
	printf("+++++++++++++++++++++++++++++++++++++\n");
}



// Print an AST_Values structure
void ast_print_values(AST_Values *values)
{
	printf("#-----Values-----#\n");
	for (int i = 0; i < values->size; i++) {
		AST_Constant *constant = values->elements[i];
		switch (constant->type) {
			case INT:
				printf("Integer value %d\n", constant->intval); break;
			case LOG:
				printf("Logical value %s\n", (constant->lval) ? "True" : "False"); break;
			case REAL:
				printf("Real value %lf\n", constant->rval); break;
			case CHAR:
				printf("Character value %c\n", constant->charval); break;
			case STR:
				printf("String value %s\n", constant->strval); break;
			case CMPLX:
				printf("Complex value with Re = %lf and Im = %lf\n",
					constant->cmplxval.re, constant->cmplxval.im); break;
			default:
				printf("Value of unknown type\n");
		}
	}
	printf("#------------------#\n");
}