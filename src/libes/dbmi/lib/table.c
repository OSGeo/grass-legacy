#include "dbmi.h"

dbTable *
db_alloc_table (ncols)
    int ncols;
{
    dbTable *table;
    int i;

    table = (dbTable *) db_malloc (sizeof(dbTable));
    if (table == NULL)
	return (table = NULL);

    db_init_table (table);

    table->columns = (dbColumn *) db_calloc (sizeof(dbColumn), ncols);
    if (table->columns == NULL)
    {
	free(table);
	return (table = NULL);
    }
    table->numColumns = ncols;
    for (i = 0; i < ncols; i++)
	db_init_column (&table->columns[i]);
    
    return table;
}

void
db_init_table (table)
    dbTable *table;
{
    db_zero ((void *)table, sizeof(dbTable));
    db_init_string (&table->tableName);
    db_init_string (&table->description);
}

void
db_free_table (table)
    dbTable *table;
{
    int i;

    db_free_string (&table->tableName);
    for (i = 0; i < table->numColumns; i++)
	db_free_column (&table->columns[i]);
    if (table->columns)
	free (table->columns);
    free (table);
}

int
db_set_table_name (table, name)
    dbTable *table;
    char *name;
{
    return db_set_string (&table->tableName, name);
}

char *
db_get_table_name (table)
    dbTable *table;
{
    return db_get_string (&table->tableName);
}

int
db_set_table_description (table, description)
    dbTable *table;
    char *description;
{
    return db_set_string (&table->description, description);
}

char *
db_get_table_description (table)
    dbTable *table;
{
    return db_get_string (&table->description);
}

db_get_table_number_of_columns(table)
    dbTable *table;
{
    return table->numColumns;
}

static void
set_all_column_privs (table, set_column_priv)
    dbTable *table;
    void (*set_column_priv)();
{
    int col, ncols;
    dbColumn *column;

    ncols = db_get_table_number_of_columns (table);
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	set_column_priv (column);
    }
}

static int
get_all_column_privs (table, get_column_priv)
    dbTable *table;
    int (*get_column_priv)();
{
    int priv, col, ncols;
    dbColumn *column;

    ncols = db_get_table_number_of_columns (table);
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	priv = get_column_priv (column);
	if (priv != DB_GRANTED)
	    return priv;
    }
    return DB_GRANTED;
}

void
db_set_table_select_priv_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_select_priv_granted);
}

void
db_set_table_select_priv_not_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_select_priv_not_granted);
}

db_get_table_select_priv (table)
    dbTable *table;
{
    return get_all_column_privs (table, db_get_column_select_priv);
}

void
db_set_table_update_priv_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_update_priv_granted);
}

void
db_set_table_update_priv_not_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_update_priv_not_granted);
}

db_get_table_update_priv (table)
    dbTable *table;
{
    return get_all_column_privs (table, db_get_column_update_priv);
}

void
db_set_table_insert_priv_granted (table)
    dbTable *table;
{
    table->insert = DB_GRANTED;
}

void
db_set_table_insert_priv_not_granted (table)
    dbTable *table;
{
    table->insert = DB_NOT_GRANTED;
}

db_get_table_insert_priv (table)
    dbTable *table;
{
    return table->insert;
}

void
db_set_table_delete_priv_granted (table)
    dbTable *table;
{
    table->delete = DB_GRANTED;
}

void
db_set_table_delete_priv_not_granted (table)
    dbTable *table;
{
    table->delete = DB_NOT_GRANTED;
}

db_get_table_delete_priv (table)
    dbTable *table;
{
    return table->delete;
}
