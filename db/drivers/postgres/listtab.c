#include <stdlib.h>
#include <string.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int db__driver_list_tables(tlist, tcount, system)
     dbString **tlist;
     int *tcount;
     int system;
{
    int i, nrows, ncols, tablecol, schemacol;
    dbString *list;
    PGresult *res;
    char buf[1000];

    init_error();
    *tlist = NULL;
    *tcount = 0;

    res = PQexec(pg_conn, "select * from pg_tables where tablename !~ 'pg_*' order by tablename");
    
    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	append_error ( "Cannot select table names\n" );
	append_error ( PQerrorMessage(pg_conn) );
	report_error();
	PQclear(res);
	return DB_FAILED;
    }

    /* Find table and schema col */
    ncols = PQnfields(res);
    schemacol = -1;
    for (i = 0; i < ncols; i++) {
	if ( strcmp(PQfname(res, i),"tablename") == 0 )
	    tablecol = i;

	if ( strcmp(PQfname(res, i),"schemaname") == 0 )
	    schemacol = i;
    }

    nrows = PQntuples(res);
    
    list = db_alloc_string_array(nrows);
    
    if (list == NULL ) {
	append_error ( "Cannot db_alloc_string_array()");
	report_error();
	return DB_FAILED;
    }

    for (i = 0; i < nrows; i++) {
	if ( schemacol >= 0 ) {
	   sprintf ( buf, "%s.%s", (char *) PQgetvalue(res, i, schemacol), 
		                   (char *) PQgetvalue(res, i, tablecol) );
	} else {
	   sprintf ( buf, "%s", (char *) PQgetvalue(res, i, tablecol) );
	}
	db_set_string(&list[i], buf );
    }

    PQclear(res);

    *tlist = list;
    *tcount = nrows;
    return DB_OK;
}
