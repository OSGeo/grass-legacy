#include <stdlib.h>
#include "gis.h"
#include "dbmi.h"

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
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

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_init_table (table)
    dbTable *table;
{
    db_zero ((void *)table, sizeof(dbTable));
    db_init_string (&table->tableName);
    db_init_string (&table->description);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
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

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_set_table_name (table, name)
    dbTable *table;
    char *name;
{
    return db_set_string (&table->tableName, name);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
char *
db_get_table_name (table)
    dbTable *table;
{
    return db_get_string (&table->tableName);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_set_table_description (table, description)
    dbTable *table;
    char *description;
{
    return db_set_string (&table->description, description);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
char *
db_get_table_description (table)
    dbTable *table;
{
    return db_get_string (&table->description);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_get_table_number_of_columns(table)
    dbTable *table;
{
    return table->numColumns;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
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

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
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

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_select_priv_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_select_priv_granted);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_select_priv_not_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_select_priv_not_granted);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_get_table_select_priv (table)
    dbTable *table;
{
    return get_all_column_privs (table, db_get_column_select_priv);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_update_priv_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_update_priv_granted);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_update_priv_not_granted (table)
    dbTable *table;
{
    set_all_column_privs (table, db_set_column_update_priv_not_granted);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_get_table_update_priv (table)
    dbTable *table;
{
    return get_all_column_privs (table, db_get_column_update_priv);
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_insert_priv_granted (table)
    dbTable *table;
{
    table->insert = DB_GRANTED;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_insert_priv_not_granted (table)
    dbTable *table;
{
    table->insert = DB_NOT_GRANTED;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_get_table_insert_priv (table)
    dbTable *table;
{
    return table->insert;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_delete_priv_granted (table)
    dbTable *table;
{
    table->delete = DB_GRANTED;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
void
db_set_table_delete_priv_not_granted (table)
    dbTable *table;
{
    table->delete = DB_NOT_GRANTED;
}

/*!
 \fn 
 \brief 
 \return 
 \param 
*/
int
db_get_table_delete_priv (table)
    dbTable *table;
{
    return table->delete;
}

/*!
 \fn 
 \brief 
 \return: 1 exist, 0 doesn't exist, -1 error
 \param 
*/
int
db_table_exists ( char *drvname, char *dbname, char *tabname)
{
    dbDriver *driver;
    dbString *names;
    int i, count, found = 0;

    driver = db_start_driver_open_database ( drvname, dbname );
    if ( driver == NULL ) {
        G_warning ( "Cannot open database '%s' by driver '%s'", dbname, drvname );
	return -1;
    }
    
    /* user tables */
    if( db_list_tables (driver, &names, &count, 0) != DB_OK) return (-1);

    for (i = 0; i < count; i++) {
	if ( G_strcasecmp( tabname, db_get_string (&names[i])) == 0 ) {
            found = 1;
	    break;
	}
    }
    db_free_string_array(names, count);
    
    if ( !found ) {    /* system tables */
	if( db_list_tables (driver, &names, &count, 1) != DB_OK) return (-1);

	for (i = 0; i < count; i++) {
	    if ( G_strcasecmp( tabname, db_get_string (&names[i])) == 0 ) {
		found = 1;
		break;
	    }
	}
	db_free_string_array(names, count);
    }
    db_close_database_shutdown_driver ( driver );

    return (found);
}
