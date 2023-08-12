#ifndef _SYMBOL_TABLE_H_
#define _SYMBOL_TABLE_H_

#include "hashtbl.h"
#include "ast.h"
#include <stdbool.h>

/****************************************************/
/********************** STRUCTS *********************/
/****************************************************/

// Struct for symbol table variable entry
typedef struct variable {
	type_t type;
	AST_Dims *dims;
	struct variable **fields;
	int no_fields;
	AST_Values *value_list;
} STBL_Variable;

// Struct for symbol table variable subprogram entry
typedef struct {
	void *placeholder;
} STBL_Subprogram;

// Enum for describing the type of a symbol table entry
typedef enum { VARIABLE, SUBPROGRAM } STBL_EntryType;

// Entries can be either variables or subprograms
typedef struct {
	STBL_EntryType entry_type; // Either VARIABLE or SUBPROGRAM
	union {
		STBL_Variable *variable;
		STBL_Subprogram *subprogram;
	};
} STBL_Entry;

/****************************************************/
/********************* FUNCTIONS ********************/
/****************************************************/

bool stbl_insert_variable(char *, AST_Dims *);
int stbl_increase_scope();
int stbl_decrease_scope();
void stbl_clear_scope();

STBL_Entry *stbl_search_scope(const char *);
STBL_Entry *stbl_search_variable(const char *);
STBL_Entry *stbl_search_subprogram(const char *);

#endif