#include <stdlib.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int db__driver_execute_immediate(sql)
     dbString *sql;
{
    PGresult *res;
    char     *str;

    init_error();

    /* Postgres supports in addition to standard escape character ' (apostrophe) also \ (basckslash)
     * as this is not SQL standard, GRASS modules cannot work escape all \ in the text
     * because other drivers do not support this feature. For example, if a text contains 
     * string \' GRASS modules escape ' by another ' and string passed to driver is \''
     * postgres takes \' as ' but second ' remains not escaped, result is error.
     * Because of this, all occurencies of \ in sql are escaped by \ */
    str = G_str_replace ( db_get_string(sql), "\\", "\\\\" );

    G_debug ( 3, "Escaped SQL: %s", str );

    res = PQexec(pg_conn, str );

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	append_error( "Cannot execute: \n" );
	append_error( str );
	append_error( "\n" );
	append_error(PQerrorMessage(pg_conn));
	report_error();
	PQclear(res);
	if ( str ) 
	    free ( str );
	return DB_FAILED;
    }
    
    if ( str ) 
	free ( str );
    PQclear(res);

    return DB_OK;
}

int db__driver_begin_transaction(void)
{
    PGresult *res;

    G_debug (2, "pg : BEGIN");

    init_error();
    res = PQexec(pg_conn, "BEGIN");

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	append_error( "Cannot 'BEGIN' transaction");
	report_error();
	PQclear(res);
	return DB_FAILED;
    }
    
    PQclear(res);

    return DB_OK;
}

int db__driver_commit_transaction(void)
{
    PGresult *res;

    G_debug (2, "pg : COMMIT");

    init_error();
    res = PQexec(pg_conn, "COMMIT");

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	append_error( "Cannot 'COMMIT' transaction" );
	report_error();
	PQclear(res);
	return DB_FAILED;
    }
    
    PQclear(res);

    return DB_OK;
}

