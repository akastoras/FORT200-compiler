#include "symbol_table.h"
#include <stdbool.h>
#include <assert.h>

HASHTBL *symbol_table;
int scope = 0;

// Insert variable id to the symbol table
bool stbl_insert_variable(const char *key, AST_Dims *dims)
{
	// Create a new entry for the variable with the name id
	STBL_Entry *entry = safe_malloc(sizeof(STBL_Entry));
	entry->entry_type = VARIABLE;
	entry->variable = safe_malloc(sizeof(STBL_Variable));
	entry->variable->dims = dims;

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
STBL_Entry *stbl_search_scope(const char *key)
{
	return hashtbl_search(symbol_table, key, scope);
}

// Search for a variable in the symbol table
STBL_Entry *stbl_search_variable(const char *key)
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

	return data;
}

// Search for an id used as a dimension
int stbl_get_dim(const char *key)
{
	// Ensure the id exists
	STBL_Entry *data = stbl_search_variable(key);
	assert(data != NULL);

	// Ensure the id is an integer constant
	AST_Values *value_list = data->variable->value_list;
	assert(value_list->size == 1 && value_list->data[0]->type == INT);

	return value_list->data[0]->value.intval;
}

// Search for a subprogram in the symbol table
STBL_Entry *stbl_search_subprogram(const char *key)
{
	// subprograms are in scope 0 and scope 0 has only subprograms
	STBL_Entry *entry = stbl_search_scope(symbol_table, key, 0);
	assert(entry == NULL || entry->entry_type == SUBPROGRAM);
	return entry;
}