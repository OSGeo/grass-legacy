#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int db__driver_list_tables(tlist, tcount, system)
     dbString **tlist;
     int *tcount;
     int system;
{
    int i, nrows;
    dbString *list;
    PGresult *res;

    init_error();
    *tlist = NULL;
    *tcount = 0;

    res = PQexec(pg_conn, "select tablename from pg_tables where tablename !~ 'pg_*' order by tablename");
    
    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	append_error ( "Cannot select table names\n" );
	append_error ( PQerrorMessage(pg_conn) );
	report_error();
	PQclear(res);
	return DB_FAILED;
    }

    nrows = PQntuples(res);
    
    list = db_alloc_string_array(nrows);
    
    if (list == NULL ) {
	append_error ( "Cannot db_alloc_string_array()");
	report_error();
	return DB_FAILED;
    }

    for (i = 0; i < nrows; i++) 
	db_set_string(&list[i], (char *) PQgetvalue(res, i, 0) );

    PQclear(res);

    *tlist = list;
    *tcount = nrows;
    return DB_OK;
}
