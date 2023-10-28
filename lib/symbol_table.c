#include "symbol_table.h"
#include "semantic.h"
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

HASHTBL *symbol_table;
int scope = 0;

// Wrapper for initialization
void stbl_create()
{
	if(!(symbol_table = hashtbl_create(10, NULL))) {
		fprintf(stderr, "ERROR: hashtbl_create() failed!\n");
		exit(EXIT_FAILURE);
	}
}

// Wrapper for destruction
void stbl_destroy()
{
	hashtbl_destroy(symbol_table);
}

int stbl_get_curr_scope()
{
	return scope;
}

// Insert variable id to the symbol table
bool stbl_insert_variable(char *key, decl_t *decl)
{
	// Create a new entry for the variable with the name id
	STBL_Entry *entry = safe_malloc(sizeof(STBL_Entry));
	entry->entry_type = VARIABLE;
	entry->decl = decl;

	return (hashtbl_insert(symbol_table, key, entry, scope) == 0);
}

// Create a subprogram in scope 0
bool stbl_insert_subprogram(char *key, AST_Subprogram *subprogram)
{
	// Create a new entry for the variable with the name id
	STBL_Entry *entry = safe_malloc(sizeof(STBL_Entry));
	entry->entry_type = SUBPROGRAM;
	entry->subprogram = subprogram;

	return (hashtbl_insert(symbol_table, key, entry, 0) == 0);
}

// Converts integer label to its corresponding string
char *itoa(int num) {
	int size = (int)((ceil(log10(num))+1)*sizeof(char));
	char *key = safe_malloc(size*sizeof(char));
	sprintf(key, "%d", num);

	return key;
}

// Insert label to the symbol table
bool stbl_insert_label(int label, AST_Statement *next_statement)
{
	char *key = itoa(label);
	
	// Create a new entry for the variable with the name id
	STBL_Entry *entry = safe_malloc(sizeof(STBL_Entry));
	entry->entry_type = LABEL;
	entry->label = safe_malloc(sizeof(label_t));
	entry->label->next_statement = next_statement;

	return (hashtbl_insert(symbol_table, key, entry, scope) == 0);
}

// Increase and get for scope variable
int stbl_increase_scope()
{
	return ++scope;
}

// Decrease and get for scope variable
int stbl_decrease_scope()
{
	return --scope;
}

// Wrapper for clear
void stbl_clear_scope()
{
	hashtbl_get(symbol_table, scope);
}

// Search for a label in the current scope
label_t *stbl_search_label_in_current_scope(int label)
{
	char *key = itoa(label);
	STBL_Entry *entry = stbl_search_current_scope(key);
	return ((entry) ? entry->label : NULL);
}

// Search for an id in the current scope
STBL_Entry *stbl_search_current_scope(const char *key)
{
	return hashtbl_search(symbol_table, key, scope);
}

// Search for an id in a given scope
STBL_Entry *stbl_search_scope(const char *key, int scope)
{
	return hashtbl_search(symbol_table, key, scope);
}

// Search for a variable in the symbol table
decl_t *stbl_search_variable(const char *key)
{
	STBL_Entry *data = NULL;
	
	// Search for a variable in every scope except the first
	// since FORT200 does not support global variables
	for (int i = scope; i >= 1; i--) {
		data = hashtbl_search(symbol_table, key, i);
		if (data != NULL) {
			assert(data->entry_type == VARIABLE);
			break;
		}
	}

	if (data != NULL)
		return data->decl;
	else
		return NULL;
}

// Search for a subprogram in the symbol table
AST_Subprogram *stbl_search_subprogram(const char *key)
{
	// subprograms are in scope 0 and scope 0 has only subprograms
	STBL_Entry *data = stbl_search_scope(key, 0);

	if (data != NULL) {
		assert(data->entry_type == SUBPROGRAM);
		return data->subprogram;
	}

	return NULL;
}