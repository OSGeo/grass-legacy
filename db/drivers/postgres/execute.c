/*****************************************************************************
*
* MODULE:       PostgreSQL driver forked from DBF driver by Radim Blazek 
*   	    	
* AUTHOR(S):    Alex Shevlakov
*
* PURPOSE:      Simple driver for reading and writing data     
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int db_driver_execute_immediate(sql)
     dbString *sql;
{
    char *s;
    int ret;

    db_set_string ( &errMsg, "" );
    s = db_get_string(sql);

    ret = execute(s, NULL);

    if (ret == DB_FAILED) {
	db_append_string ( &errMsg, "Error in db_execute_immediate()\n");
	report_error(  db_get_string (&errMsg) );
	return DB_FAILED;
    }

    return DB_OK;
}

int db_driver_begin_transaction(void)
{
    PGresult *res;

    G_debug (2, "pg : BEGIN");
    res = PQexec(pg_conn, "BEGIN");

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	report_error( "Cannot 'BEGIN' transaction");
	PQclear(res);
	return DB_FAILED;
    }
    
    PQclear(res);

    return DB_OK;
}

int db_driver_commit_transaction(void)
{
    PGresult *res;

    G_debug (2, "pg : COMMIT");
    res = PQexec(pg_conn, "COMMIT");

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	report_error( "Cannot 'COMMIT' transaction" );
	PQclear(res);
	return DB_FAILED;
    }
    
    PQclear(res);

    return DB_OK;
}

