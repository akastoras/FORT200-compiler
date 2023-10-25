#ifndef _SYMBOL_TABLE_H_
#define _SYMBOL_TABLE_H_

#include "hashtbl.h"
#include "ast.h"
#include <stdbool.h>

/****************************************************/
/********************** STRUCTS *********************/
/****************************************************/

typedef struct {
	AST_Statement *next_statement;
} label_t;

// Enum for describing the type of a symbol table entry
typedef enum { VARIABLE, SUBPROGRAM, LABEL } STBL_EntryType;

// Entries can be either variables or subprograms
typedef struct {
	STBL_EntryType entry_type; // Either VARIABLE or SUBPROGRAM
	union {
		decl_t *decl;
		AST_Subprogram *subprogram;
		label_t *label;
	};
} STBL_Entry;

/****************************************************/
/********************* FUNCTIONS ********************/
/****************************************************/

void stbl_create();
void stbl_destroy();
int stbl_get_curr_scope();
bool stbl_insert_variable(char *, decl_t *);
bool stbl_insert_subprogram(char *, AST_Subprogram *);
bool stbl_insert_label(int, AST_Statement *);
int stbl_increase_scope();
int stbl_decrease_scope();
void stbl_clear_scope();

STBL_Entry *stbl_search_scope(const char *, int);
label_t *stbl_search_label_in_current_scope(int);
STBL_Entry *stbl_search_current_scope(const char *key);
decl_t *stbl_search_variable(const char *);
AST_Subprogram *stbl_search_subprogram(const char *);

#endif