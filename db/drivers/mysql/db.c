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

typedef struct { 
    char *host,  *socket, *dbname, *user, *password;
    int  port;
} MYCONN;

/* Parse connection string in form: 1) 'database_name'
*  2) 'host=xx,port=xx,socket=xx,dbname=xx,user=xx,password=xx'
*  
*  returns:  0 OK
*           -1 error
*/
int parse_conn ( char *str, MYCONN *myconn )
{
    int  i;
    char **tokens, delm[2];
    
    /* reset */
    myconn->host = NULL;
    myconn->port = 0;
    myconn->socket = NULL;
    myconn->dbname = NULL;
    myconn->user = NULL;
    myconn->password = NULL;
 
    G_debug (3, "parse_conn : %s", str ); 
    
    if ( strchr(str, '=') == NULL ) { /*db name only */
	myconn->dbname = G_store ( str );
    } else {
	delm[0] = ','; delm[1] = '\0';
        tokens = G_tokenize ( str, delm );
	i = 0;
	while ( tokens[i] ) {
	   G_debug (3, "token %d : %s", i, tokens[i] ); 
	   if ( strncmp(tokens[i], "host", 4 ) == 0 )
	       myconn->host = G_store ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "port", 4 ) == 0 )
	       myconn->port = atoi ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "socket", 6 ) == 0 )
	       myconn->socket = G_store ( tokens[i] + 7 );
	   else if ( strncmp(tokens[i], "dbname", 6 ) == 0 )
	       myconn->dbname = G_store ( tokens[i] + 7 );
	   else if ( strncmp(tokens[i], "user", 4 ) == 0 )
	       G_warning ( "'user' in database definition is not supported, use db.login" );
	      /* myconn->user = G_store ( tokens[i] + 5 ); */
	   else if ( strncmp(tokens[i], "password", 8 ) == 0 )
	       G_warning ( "'password' in database definition is not supported, use db.login" );
	      /* myconn->password = G_store ( tokens[i] + 9 ); */
	   else 
               G_warning ( "Unknown option in database definition for mysql: '%s'", tokens[i] );
	   
	   i++;
	}
	G_free_tokens ( tokens );	
    }

    return 0;
}

int db__driver_open_database(handle)
     dbHandle *handle;
{
    char *name, *user, *password;
    char emsg[MYSQL_MSG];
    dbConnection connection;
    MYCONN myconn;
    MYSQL *ret_conn;

    MYSQL_RES *res;
    MYSQL_ROW row;

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

    G_debug(3, "db_driver_open_database() driver = mysql database definition = '%s'", name );

    parse_conn ( name, &myconn );

    G_debug(3, "host = %s, port = %d, socket = %s, dbname = %s, user = %s, password = %s", 
	        myconn.host, myconn.port, myconn.socket, myconn.dbname, myconn.user, myconn.password ); 
	    
    strcpy(db.name, myconn.dbname);

    db_get_login ( "mysql", name, &user, &password );

    /* Try to connect first maybe without user/password */
    mysql_init(&mysql_conn);
    ret_conn = mysql_real_connect(&mysql_conn, myconn.host, user, password, myconn.dbname, 
		myconn.port, myconn.socket, 0);

    free ( user );
    free ( password );

    if ( ret_conn == NULL ) {  
	  snprintf(emsg, sizeof(emsg), "mysql_real_connect() error (%d): %s\n",
					mysql_errno(&mysql_conn), mysql_error(&mysql_conn));
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

int db__driver_close_database()
{
    int i;

    for (i = 0; i < db.ntables; i++) {

	free_table(i);
    }
    free(db.tables);

    mysql_close(&mysql_conn);

    return DB_OK;
}
