#include "symbol_table.h"
#include "semantic.h"
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

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

// Insert variable id to the symbol table
bool stbl_insert_variable(char *key, decl_t *decl)
{
	// Create a new entry for the variable with the name id
	STBL_Entry *entry = safe_malloc(sizeof(STBL_Entry));
	entry->entry_type = VARIABLE;
	entry->decl = decl;

	return (hashtbl_insert(symbol_table, key, entry, scope) == 0);
}

// Create a sub
// bool stbl_insert_subprogram()
// {

// }

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
STBL_Entry *stbl_search_subprogram(const char *key)
{
	// subprograms are in scope 0 and scope 0 has only subprograms
	STBL_Entry *entry = stbl_search_scope(key, 0);
	assert(entry == NULL || entry->entry_type == SUBPROGRAM);
	return entry;
}

int stbl_get_int_initVal(char *id)
{
	decl_t *decl = stbl_search_variable(id);
	int error;

	error = SEM_check_existing_variable(decl);
	if (!error)
		error = SEM_check_initial_value_exists(decl);
	if (!error)
		error = SEM_check_decl_datatype_simple(decl->datatype, INT, id);

	if (!error)
		return decl->initial_value->elements[0]->intval;
	else
		return 1; // A value to continue with compilation
}