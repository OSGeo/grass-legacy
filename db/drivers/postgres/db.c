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
#include "../dialog/dbd.h"

typedef struct { 
    char *host, *port, *options, *tty, *dbname, *user, *password;
} PGCONN;

/* Parse connection string in form: 1) 'database_name'
*  2) 'host=xx,port=xx,dbname=xx,user=xx,password=xx'
*  
*  returns:  0 OK
*           -1 error
*/
int parse_conn ( char *str, PGCONN *pgconn )
{
    int  i;
    char **tokens, delm[2];
    
    /* reset */
    pgconn->host = NULL;
    pgconn->port = NULL;
    pgconn->options = NULL;
    pgconn->tty = NULL;
    pgconn->dbname = NULL;
    pgconn->user = NULL;
    pgconn->password = NULL;
 
    G_debug (3, "parse_conn : %s", str ); 
    
    if ( strchr(str, '=') == NULL ) { /*db name only */
	pgconn->dbname = G_store ( str );
    } else {
	delm[0] = ','; delm[1] = '\0';
        tokens = G_tokenize ( str, delm );
	i = 0;
	while ( tokens[i] ) {
	   G_debug (3, "token %d : %s", i, tokens[i] ); 
	   if ( strncmp(tokens[i], "host", 4 ) == 0 )
	       pgconn->host = G_store ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "port", 4 ) == 0 )
	       pgconn->port = G_store ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "options", 7 ) == 0 )
	       pgconn->options = G_store ( tokens[i] + 8 );
	   else if ( strncmp(tokens[i], "tty", 3 ) == 0 )
	       pgconn->tty = G_store ( tokens[i] + 4 );
	   else if ( strncmp(tokens[i], "dbname", 6 ) == 0 )
	       pgconn->dbname = G_store ( tokens[i] + 7 );
	   else if ( strncmp(tokens[i], "user", 4 ) == 0 )
	       pgconn->user = G_store ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "password", 8 ) == 0 )
	       pgconn->password = G_store ( tokens[i] + 9 );
	   else 
               G_warning ( "Unknown option in database definition for postgres: '%s'", tokens[i] );
	   
	   i++;
	}
	G_free_tokens ( tokens );	
    }

    return 0;
}

int db_driver_open_database(handle)
     dbHandle *handle;
{
    char *name, emsg[PG_MSG];
    dbConnection connection;
    PGCONN pgconn;

    int i;

    PGresult *res;
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

    G_debug(3, "db_driver_open_database() driver=pg database definition = '%s'", name );

    parse_conn ( name, &pgconn );
    
    G_debug(3, "host = %s, port = %s, options = %s, tty = %s, dbname = %s, user = %s, password = %s",
                pgconn.host, pgconn.port, pgconn.options, pgconn.tty,  
		pgconn.dbname, pgconn.user, pgconn.password );

    pg_conn = PQsetdbLogin( pgconn.host, pgconn.port, pgconn.options, pgconn.tty, 
		            pgconn.dbname, pgconn.user, pgconn.password );
    
    if (PQstatus(pg_conn) == CONNECTION_BAD) {
        if ( pgconn.user == NULL || strlen(pgconn.user) == 0 || 
	     pgconn.password == NULL || strlen(pgconn.password) == 0 ) {
	   /* Ask user for login/password */
	   G_debug (3, "User/password missing");
	   if ( dbd_user ( "pg", name, &pgconn.user, &pgconn.password ) < 0 ) {
		snprintf(emsg, sizeof(emsg), "cannot get user/password\n" );
		report_error(emsg);
		return DB_FAILED;
	   }
	   G_debug ( 3, "user =  %s", pgconn.user ); 
	   
           pg_conn = PQsetdbLogin( pgconn.host, pgconn.port, pgconn.options, pgconn.tty, 
	 	                    pgconn.dbname, pgconn.user, pgconn.password );

	}
    }

    if (PQstatus(pg_conn) == CONNECTION_BAD) {
	snprintf(emsg, sizeof(emsg), "Error: connect Postgres: %s\n", PQerrorMessage(pg_conn));
	report_error(emsg);
	PQfinish(pg_conn);
	return DB_FAILED;
    }

    res = PQexec(pg_conn,
		 "select tablename from pg_tables where tablename !~ 'pg_*' order by tablename");

    if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	snprintf(emsg, sizeof(emsg), "Error: select Postgres: %s\n",
		 PQerrorMessage(pg_conn));
	report_error(emsg);
	PQclear(res);
	PQfinish(pg_conn);
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

	free_table(i);
    }
    free(db.tables);

    PQfinish(pg_conn);

    return DB_OK;
}
