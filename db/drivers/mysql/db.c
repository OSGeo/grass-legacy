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

int db_driver_open_database(handle)
     dbHandle *handle;
{
    char *name;
    char emsg[MYSQL_MSG];
    dbConnection connection;

    MYSQL_RES *res;
    MYSQL_ROW row;
    char *mysqlhost;

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

    mysqlhost = G__getenv("DB_HOST");

    mysql_init(&mysql_conn);
    if (mysql_real_connect
	(&mysql_conn, mysqlhost, connection.user, connection.password,
	 db.name, 0, NULL, 0) == NULL) {
	snprintf(emsg, sizeof(emsg), "Error: connect Mysql: %s\n",
		 mysql_error(&mysql_conn));
	report_error(emsg);
	return DB_FAILED;
    }

    if ((res = mysql_list_tables(&mysql_conn, NULL)) == NULL) {
	snprintf(emsg, sizeof(emsg), "Error: list tables: %s\n",
		 mysql_error(&mysql_conn));
	report_error(emsg);
	return DB_FAILED;
    }

    while ((row = mysql_fetch_row(res)) != NULL)
	add_table(row[0]);

    mysql_free_result(res);

    return DB_OK;
}

int db_driver_close_database()
{
    int i;

    for (i = 0; i < db.ntables; i++) {

	free_table(i);
    }
    free(db.tables);

    mysql_close(&mysql_conn);

    return DB_OK;
}
