#include "globals.h"

compile_cursor (c, stmt, dbc)
    mycursor *c;
    dbString *stmt;
    dbCursor *dbc;
{
    SQLTRCD rcd;
    char *s;
    dbTable *table;

    s = db_get_string(stmt);

    if (db_test_cursor_mode_scroll(dbc))
    {
	/* to get a scroll cursor, use "result set mode"
	 * if that fails, revert to non-scroll mode 
	 */
	rcd = sqlsrs(c->database_cursor);
	if(rcd)
	{
	    db_unset_cursor_mode_scroll(dbc);
	}
    }
    /* turn off restriction set mode turned on by sqlsrs() */
    sqlspr(c->database_cursor);

    /* compile the select. note compile must come after the call to sqlspr() */
    rcd = sqlcom (c->database_cursor, s, NULL_TERMINATED);
    if (rcd)
    {
	report_error_with_carot(c->database_cursor, rcd, s);
	return DB_FAILED;
    }

/* NOTE:may have to test for the dbc type: SELECT, UPDATE, INSERT */

    if(describe (c->database_cursor, &table) != DB_OK)
	return DB_FAILED;
    db_set_cursor_table (dbc, table);

/* now execute the stmt */
    rcd = sqlexe (c->database_cursor);
    if (rcd)
    {
	report_error (rcd, NULL);
	return DB_FAILED;
    }

/* if scroll mode, get number of rows in the result set */
    if (db_test_cursor_mode_scroll(dbc))
    {
	rcd = sqlnrr (c->database_cursor, &c->nrows);
	if (rcd)
	{
	    report_error (rcd, "sqlnrr()");
	    return DB_FAILED;
	}
    }

/* if this cursor is INSENSITIVE, then turn on FETCHTHROUGH */
    if (db_test_cursor_mode_insensitive(dbc))
    {
	SQLTDPV on = 1;
	rcd = sqlset (c->database_cursor, SQLPFT, &on, 0);
	if (rcd)
	{
	    db_unset_cursor_mode_insensitive(dbc);
	}
    }

    return DB_OK;
}
