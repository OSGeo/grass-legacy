#include "globals.h"

int
db_driver_open_insert_cursor (dbc)
    dbCursor *dbc;
{
    mycursor *c;

/* allocate cursor */
    c = open_mycursor();
    if (c == NULL)
	return DB_FAILED;

    db_set_cursor_type_insert(dbc);

    if(compile_insert_cursor(c, dbc) != DB_OK) {
	close_mycursor(c);
	return DB_FAILED;
    }

/* set dbCursor's token for my cursor */
    db_set_cursor_token(dbc, c->token);


    /* return OK */
    return DB_OK;
}

int
compile_insert_cursor (c, dbc)
    mycursor *c;
    dbCursor *dbc;
{
    char num[20];
    char *s;
    dbString cmd;
    dbTable *table;
    dbColumn *column;
    int ncols, col, bindvar;
    SQLTRCD rcd;	/*return code */

    db_init_string (&cmd);

    table = db_get_cursor_table (dbc);
    ncols = db_get_table_number_of_columns(table);

    db_append_string (&cmd, "INSERT INTO ");
    db_append_string (&cmd, db_get_table_name (table));

    /* column names */

    db_append_string (&cmd, " (");
    bindvar = 0;
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	if(skip_column(column))
	    continue;
	if(bindvar++)
	    db_append_string (&cmd, ",");
	db_append_string (&cmd, db_get_column_name(column));
    }

    /* variables */

    db_append_string (&cmd, ") VALUES (");
    bindvar = 0;
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	if(skip_column(column))
	    continue;
	if(bindvar++)
	    db_append_string (&cmd, ",");
	sprintf (num, ":%d", bindvar);
	db_append_string (&cmd, num);
    }
    db_append_string (&cmd, ")");

    s = db_get_string(&cmd);
    rcd = sqlcom (c->database_cursor, s, NULL_TERMINATED);
    if (rcd)
    {
	report_error_with_carot(c->database_cursor, rcd, s);
	return DB_FAILED;
    }
    return DB_OK;
}

db_driver_insert (dbc)
    dbCursor *dbc;
{
    dbTable *table;
    dbColumn *column;
    int ncols, col, bindvar;
    mycursor *c;
    SQLTRCD rcd;	/*return code */

    c = (mycursor *) db_find_token (db_get_cursor_token (dbc));
    if (c == NULL)
    {
	db_error ("invalid cursor");
	return DB_FAILED;
    }

    table = db_get_cursor_table (dbc);
    ncols = db_get_table_number_of_columns(table);

    bindvar = 0;
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	if (skip_column(column))
	    continue;
	if(rcd = bind_column_for_insert (c->database_cursor, ++bindvar, column))
	    return DB_FAILED;
    }
    rcd = sqlexe (c->database_cursor);
    if (rcd)
    {
	report_error (rcd, "db_insert() failure");
	return DB_FAILED;
    }
    return DB_OK;
}

static
skip_column(column)
    dbColumn *column;
{
    return db_test_column_use_default_value(column);
}
