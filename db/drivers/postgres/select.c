#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int db_driver_open_select_cursor(sel, dbc, mode)
     dbString *sel;
     dbCursor *dbc;
     int mode;
{
    PGresult *res;
    cursor *c;
    dbTable *table;

    init_error();

    /* Set datetime style */
    res = PQexec(pg_conn, "SET DATESTYLE TO ISO");

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	append_error( "Cannot set DATESTYLE\n" );
	report_error();
	PQclear(res);
	return DB_FAILED;
    }
    
    PQclear(res);

    /* allocate cursor */
    c = alloc_cursor();
    if (c == NULL)
	return DB_FAILED;

    db_set_cursor_mode(dbc, mode);
    db_set_cursor_type_readonly(dbc);

    c->res = PQexec(pg_conn, db_get_string(sel) );
    
    if (!c->res || PQresultStatus(c->res) != PGRES_TUPLES_OK) {
	append_error("Cannot select: \n");
	append_error(db_get_string(sel) );
	append_error( "\n" );
	append_error(PQerrorMessage(pg_conn));
	report_error();
	PQclear(c->res);
	return DB_FAILED;
    }

    if ( describe_table( c->res, &table, c) == DB_FAILED ) {
	append_error("Cannot describe table\n");
	report_error();
	PQclear(res);
	return DB_FAILED;
    }

    c->nrows = PQntuples(c->res);
    c->row = -1;

    /* record table with dbCursor */
    db_set_cursor_table(dbc, table);

    /* set dbCursor's token for my cursor */
    db_set_cursor_token(dbc, c->token);

    return DB_OK;
}
