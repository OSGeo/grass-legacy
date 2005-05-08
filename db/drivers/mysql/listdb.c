/*****************************************************************************
*
* MODULE:       MySQL driver forked from DBF driver by Radim Blazek 
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

int db__driver_list_databases(dbpath, npaths, dblist, dbcount)
     dbString *dbpath;
     int npaths;
     dbHandle **dblist;
     int *dbcount;
{
    char *emsg;
    char *name;
    dbConnection connection;
    int i;
    MYSQL_RES *res;
    MYSQL_ROW row;
    char *mysqlhost;

    int rec_num = 0;

    dbHandle *list;

    *dblist = NULL;
    *dbcount = 0;

    db_get_connection(&connection);

    name = connection.databaseName;
    strcpy(db.name, name);

    mysqlhost = G__getenv("DB_HOST");

    mysql_init(&mysql_conn);

    if (mysql_real_connect
	(&mysql_conn, mysqlhost, connection.user, connection.password,
	 db.name, 0, NULL, 0) == NULL) {
	G_asprintf(&emsg, "Error: connect Mysql: %s\n",
		 mysql_error(&mysql_conn));
	report_error(emsg);
        G_free(emsg);

	return DB_FAILED;
    }


    if ((res = mysql_list_dbs(&mysql_conn, NULL)) == NULL) {
	G_asprintf(&emsg, "Error: list databases: %s\n",
		 mysql_error(&mysql_conn));
	report_error(emsg);
        G_free(emsg);

	return DB_FAILED;
    }

    rec_num = mysql_num_rows(res);

    list = db_alloc_handle_array(rec_num);
    if (list == NULL) {
	report_error("db_alloc_handle_array()");
	return DB_FAILED;
    }


    i = 0;
    while ((row = mysql_fetch_row(res)) != NULL) {
	db_init_handle(&list[i]);
	if (db_set_handle(&list[i], row[0], NULL) != DB_OK) {
	    report_error("db_set_handle()");
	    db_free_handle_array(list, rec_num);
	    return DB_FAILED;
	}
	i++;
    }

    mysql_free_result(res);
    mysql_close(&mysql_conn);


    *dblist = list;
    *dbcount = rec_num;


    return DB_OK;
}
