#include "globals.h"

int
db_driver_open_update_cursor(table_name, select_string, dbc, mode)
    dbString *table_name;
    dbString *select_string;
    dbCursor *dbc;
    int mode;
{
    mycursor *c;

/* allocate cursor */
    c = open_mycursor();
    if (c == NULL)
	return DB_FAILED;

    db_set_cursor_mode(dbc,mode);
    db_set_cursor_type_update(dbc);

    if(compile_cursor(c, select_string, dbc) != DB_OK) {
	close_mycursor(c);
	return DB_FAILED;
    }

/* set dbCursor's token for my cursor */
    db_set_cursor_token(dbc, c->token);

/* copy the table name into mycursor */
    db_copy_string (&c->tableName, table_name);

    /* return OK */
    return DB_OK;
}

int
db_driver_bind_update (dbc)
    dbCursor *dbc;
{
    char num[20];
    char *s;
    dbString cmd;
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

    db_init_string (&cmd);

    db_append_string (&cmd, "UPDATE ");
    db_append_string (&cmd, db_get_string (&c->tableName));
    db_append_string (&cmd, " SET ");

    table = db_get_cursor_table (dbc);
    ncols = db_get_table_number_of_columns(table);

    bindvar = 0;
    for (col = 0; col < ncols; col++)
    {
	if (skip_column(dbc, col))
	    continue;
	column = db_get_table_column (table, col);
	if (bindvar++)
	    db_append_string (&cmd, ",");
	db_append_string (&cmd, db_get_column_name (column));
	sprintf (num, " = :%d ", bindvar);
	db_append_string (&cmd, num);
    }
    db_append_string (&cmd, " WHERE CURRENT OF ");
    db_append_string (&cmd, c->name);

    if(open_mycursor_update_cursor(c) != DB_OK)
	return DB_FAILED;

    s = db_get_string(&cmd);
    rcd = sqlcom (c->update_cursor, s, NULL_TERMINATED);
    if (rcd)
    {
	report_error_with_carot(c->update_cursor, rcd, s);
	return DB_FAILED;
    }
    return DB_OK;
}

db_driver_update (dbc)
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
	if (skip_column(dbc, col))
	    continue;
	column = db_get_table_column (table, col);
	if(rcd = bind_column_for_update (c->update_cursor, ++bindvar, column))
	    return DB_FAILED;
    }
    rcd = sqlexe (c->update_cursor);
    if (rcd)
    {
	report_error (rcd, "db_update() failure");
	return DB_FAILED;
    }
    return DB_OK;
}

static
skip_column (dbc, col)
    dbCursor *dbc;
    int col;
{
    return !db_test_cursor_column_for_update(dbc, col);
}
