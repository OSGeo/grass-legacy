#include <stdlib.h>
#include <string.h>
#include <gis.h>
#include <dbmi.h>
#include "globals.h"
#include "proto.h"

/* Parse connection string in form: 1) 'database_name'
*  2) 'host=xx,port=xx,dbname=xx,user=xx,password=xx'
*  
*  returns:  DB_OK     - OK
*            DB_FAILED - error
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
	   else {
	       append_error ( "Unknown option in database definition for postgres: ");
	       append_error ( tokens[i] );
	       return DB_FAILED;
	   }
	   i++;
	}
	G_free_tokens ( tokens );	
    }

    return DB_OK;
}

