#ifndef _AST_H_
#define _AST_H_

#include <stdbool.h>
#include <stddef.h>

/****************************************************/
/********************** STRUCTS *********************/
/****************************************************/

// Value representing the type of an expression
typedef enum {
	INT, LOG, REAL, CHAR, STR, CMPLX, REC
} type_t;

// Values representing the op used as sign or addop
typedef enum {MINUS=-1, NONE=0, PLUS=1} AST_Sign;

// Representation of complex as two reals
typedef struct {
	double re;
	double im;
} complex_t;

// Struct representing a generic expression's type and value 
typedef struct {
	// Value of the expression, accessing the field
	// dictated by the type
	union  {
		int  intval;
		bool  lval;
		double rval;
		char charval;
		char *strval;
		complex_t cmplxval;
	};
	// Type of the expression's result
	type_t type;
} AST_Constant;

typedef type_t AST_Type;

// Struct containing an array of pointers to AST_Constant
// used to represent values and value_list
typedef struct {
	int size;
	AST_Constant **elements;
} AST_Values;

// Struct representing the intialization of a variable
typedef struct {
	char *id;
	AST_Values *value_list;
} init_val_t;

// Struct containing array of pointers to AST_Values
// used to represent vals
typedef struct {
	int size;
	init_val_t **elements;
} AST_Vals;

// Struct containing info for a dim array
typedef struct {
	int size;
	int *elements;
} AST_Dims;

// Value representing the type of an undefined variable
typedef enum {SCALAR, ARRAY, LIST} AST_UndefVar_Type;

// Struct representing an undefined variable. It contains its type, its dimensions
//  and a pointer to the same struct type. The last is used to create a list
typedef struct undef_var_t {
	AST_UndefVar_Type type;
	AST_Dims *dims;
	int list_depth; // No need for linked-list of UndefVars, just keep a counter
	char *id;
} AST_UndefVar;

// Struct containing an array of pointers to AST_UndefVar
// used to represent vars
typedef struct {
	int size;
	AST_UndefVar **elements;
} AST_Vars;

// Strict containing information about a field.
// When a field is a record, it holds an array of its subfields
typedef struct field {
	AST_Vars *vars; // Variables with that type
	/* The type */
	type_t type;
	int size;
	struct field **fields;
} AST_Field;

// Struct containing an array of fields
typedef struct {
	int size;
	AST_Field **elements;
} AST_Fields;

// General type that covers all data types
typedef struct {
	type_t type;
	AST_Fields *fields; // Only if type == REC
} AST_GeneralType;

// Struct for the declaration of a single id
typedef struct {
	AST_UndefVar *variable; // Many variables (SCALAR, ARRAY or LIST)
	AST_GeneralType *datatype; // The common datatype of all vars
	init_val_t *initial_value; // Same index as vars
} decl_t;

typedef struct {
	int size;
	decl_t **declarations;
} AST_Decls;


/****************************************************/
/********************* FUNCTIONS ********************/
/****************************************************/
void *safe_malloc(size_t);
void *safe_realloc(void *, size_t);

AST_Constant *ast_get_ICONST(int);
AST_Constant *ast_get_RCONST(double);
AST_Constant *ast_get_CCONST(char);
AST_Constant *ast_get_LCONST(bool);
AST_Constant *ast_get_CMPLX(double, AST_Sign, double);

AST_Sign ast_get_sign(AST_Sign);

AST_Constant *ast_get_value(AST_Sign, AST_Constant *);
AST_Constant *ast_get_string(char *);

AST_Values *ast_insert_value_to_values(AST_Values *, AST_Constant *);

AST_Vals *ast_insert_val_to_vals(AST_Vals *, char *, AST_Values *);
AST_Dims *ast_insert_dim_to_dims(AST_Dims *, int);

AST_UndefVar *ast_get_undef_var(AST_UndefVar_Type, char *, AST_Dims *, AST_UndefVar *);

AST_Vars *ast_insert_var_to_vars(AST_Vars *, AST_UndefVar *);

AST_Field *ast_get_field(type_t, AST_Vars *, AST_Fields *);

AST_Decls *ast_insert_decl_to_decls(AST_Decls *, type_t, AST_Fields *, AST_Vars *);


void ast_print_values(AST_Values *);
void ast_print_decls(AST_Decls *);


#endif