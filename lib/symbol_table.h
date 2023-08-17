#ifndef _SYMBOL_TABLE_H_
#define _SYMBOL_TABLE_H_

#include "hashtbl.h"
#include "ast.h"
#include <stdbool.h>

/****************************************************/
/********************** STRUCTS *********************/
/****************************************************/

typedef void * header_t; // Placeholder

// Enum for describing the type of a symbol table entry
typedef enum { VARIABLE, SUBPROGRAM } STBL_EntryType;

// Entries can be either variables or subprograms
typedef struct {
	STBL_EntryType entry_type; // Either VARIABLE or SUBPROGRAM
	union {
		decl_t *decl;
		header_t *subprogram;
	};
} STBL_Entry;

/****************************************************/
/********************* FUNCTIONS ********************/
/****************************************************/

void stbl_create();
void stbl_destroy();
bool stbl_insert_variable(char *, decl_t *);
int stbl_increase_scope();
int stbl_decrease_scope();
void stbl_clear_scope();

STBL_Entry *stbl_search_scope(const char *, int);
STBL_Entry *stbl_search_current_scope(const char *key);
decl_t *stbl_search_variable(const char *);
STBL_Entry *stbl_search_subprogram(const char *);
int stbl_get_int_initVal(char *);
int stbl_get_curr_scope();

#endif