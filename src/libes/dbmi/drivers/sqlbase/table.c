#include "globals.h"

/* NAME: db_driver_create_table 
 * INPUT:
 * OUTPUT:
 * PROCESSING: issue a CREATE TABLE tableName (column data_type  [ NOT NULL ], ... ) via EXECUTE IMMEDIATE
 */
db_driver_create_table (table)
    dbTable *table;
{
    dbString cmd;
    int stat;
    dbColumn *column;
    char *s;
    int col, ncols;

    db_init_string(&cmd);
    db_append_string(&cmd, "CREATE TABLE ");
    db_append_string(&cmd, db_get_table_name(table));
    db_append_string(&cmd, " (");
    ncols = db_get_table_number_of_columns(table);
    for (col = 0; col < ncols; col++) 
    {
	if (col) db_append_string(&cmd,",");
	column = db_get_table_column(table, col);
	db_append_string(&cmd, db_get_column_name(column));
	db_append_string(&cmd, " ");
	if(!build_column_type_info(column, &cmd))
	{
	    stat = DB_FAILED;
	    goto done;
	}
	if (!db_test_column_null_allowed(column))
	    db_append_string(&cmd, " NOT NULL ");
    }
    db_append_string(&cmd, ")");

    stat = execute_immediate (&cmd);
    if(stat != DB_OK)
	goto done;
    s = db_get_table_description(table);
    if(*s)
	add_remarks_to_table (db_get_table_name(table), s);
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	s = db_get_column_description (column);
	if (*s)
	{
	    add_remarks_to_column (
		db_get_table_name(table),
		db_get_column_name(column),
		s
	    );
	}
    }
done:
    db_free_string(&cmd);
    return stat;
}

/* NAME: db_driver_drop_table 
 * INPUT: name of the table to be dropped
 * OUTPUT:
 * PROCESSING: issue a DROP TABLE name via EXECUTE IMMEDIATE
 */
db_driver_drop_table (name)
    dbString *name;
{
    dbString cmd;
    int stat;
    char *s;

    s =  db_get_string(name);
    db_init_string (&cmd);
    db_append_string (&cmd, "DROP ");
    if (is_view (s))
	db_append_string (&cmd, "VIEW ");
    else
	db_append_string (&cmd, "TABLE ");
    db_append_string (&cmd, s);
    stat = execute_immediate (&cmd);
    db_free_string (&cmd);
    return stat;;
}
