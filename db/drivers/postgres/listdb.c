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

int db_driver_list_databases(dbpath, npaths, dblist, dbcount)
     dbString *dbpath;
     int npaths;
     dbHandle **dblist;
     int *dbcount;
{
    char emsg[PG_MSG];
    int i;

    PGresult *res;
    char *pghost;
    int rec_num = 0;

    dbHandle *list;

    *dblist = NULL;
    *dbcount = 0;

    pghost = G__getenv("DB_HOST");
    pg_conn = PQsetdb(pghost, NULL, NULL, NULL, "template1");

    if (PQstatus(pg_conn) == CONNECTION_BAD) {
	snprintf(emsg, sizeof(emsg), "Error: connect Postgres: %s\n",
		 PQerrorMessage(pg_conn));
	report_error(emsg);
	PQfinish(pg_conn);
	return DB_FAILED;
    }

    res = PQexec(pg_conn, "select datname from pg_database");

    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	snprintf(emsg, sizeof(emsg), "Error: select Postgres: %s\n",
		 PQerrorMessage(pg_conn));
	report_error(emsg);
	PQclear(res);
	PQfinish(pg_conn);
	return DB_FAILED;
    }

    rec_num = PQntuples(res);

    list = db_alloc_handle_array(rec_num);
    if (list == NULL) {
	report_error("db_alloc_handle_array()");
	return DB_FAILED;
    }


    for (i = 0; i < rec_num; i++)
    {
	db_init_handle(&list[i]);
	if (db_set_handle(&list[i], PQgetvalue(res, i, 0), NULL) != DB_OK) {
	    report_error("db_set_handle()");
	    db_free_handle_array(list, rec_num);
	    return DB_FAILED;
	}
    }

    PQclear(res);
    PQfinish(pg_conn);


    *dblist = list;
    *dbcount = rec_num;


    return DB_OK;
}
