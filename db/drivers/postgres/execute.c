#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int db_driver_execute_immediate(sql)
     dbString *sql;
{
    PGresult *res;

    init_error();

    res = PQexec(pg_conn, db_get_string(sql) );

    if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	append_error( "Cannot execute: \n" );
	append_error( db_get_string(sql) );
	append_error( "\n" );
	append_error(PQerrorMessage(pg_conn));
	report_error();
	PQclear(res);
	return DB_FAILED;
    }
    
    PQclear(res);

    return DB_OK;
}

int db_driver_begin_transaction(void)
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

int db_driver_commit_transaction(void)
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

