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
#include <stdlib.h>
#include <string.h>
#include <gis.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int db_driver_open_database(handle)
     dbHandle *handle;
{
    char *name, emsg[PG_MSG];
    dbConnection connection;

    int i;

    PGresult *res;
    char *pghost;
    int rec_num;


    db.name[0] = '\0';
    db.tables = NULL;
    db.atables = 0;
    db.ntables = 0;

    db_get_connection(&connection);
    name = db_get_handle_dbname(handle);

    /* if name is empty use connection.databaseName */
    if (strlen(name) == 0) {
	name = connection.databaseName;
    }

    strcpy(db.name, name);

    pghost = G__getenv("DB_HOST");
    pg_conn = PQsetdb(pghost, NULL, NULL, NULL, db.name);

    if (PQstatus(pg_conn) == CONNECTION_BAD) {
	snprintf(emsg, sizeof(emsg), "Error:connect Postgres:%s\n",
		 PQerrorMessage(pg_conn));
	report_error(emsg);
	return DB_FAILED;
    }

    res = PQexec(pg_conn,
		 "select tablename from pg_tables where tablename !~ 'pg_*' order by tablename");

    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
	snprintf(emsg, sizeof(emsg), "Error:select Postgres:%s\n",
		 PQerrorMessage(pg_conn));
	report_error(emsg);
	return DB_FAILED;
    }

    rec_num = PQntuples(res);

    for (i = 0; i < rec_num; i++)
	add_table(PQgetvalue(res, i, 0));

    PQclear(res);

    return DB_OK;
}

int db_driver_close_database()
{
    int i;

    for (i = 0; i < db.ntables; i++) {
/*	save_table (i);*/
	free_table(i);
    }
    free(db.tables);

    PQfinish(pg_conn);

    return DB_OK;
}
