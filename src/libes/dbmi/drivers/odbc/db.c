#include <gis.h>
#include <dbmi.h>
#include "odbc.h"
#include "globals.h"
#include "proto.h" 

db_driver_open_database (handle)
    dbHandle *handle;
{
    char        *name, msg[OD_MSG], emsg[DB_MSG];
    SQLRETURN   ret; 
    SQLINTEGER  err;
    dbConnection connection;

    // Open connection
    if ( open_connection() != DB_OK )
        return DB_FAILED;  

    db_get_connection( &connection );
    name = db_get_handle_dbname(handle);
    
    /* if name is empty use connection.databaseName*/
    if( strlen(name) == 0 )
    {
        name = connection.databaseName;
    } 

    // Connect to the datasource 
    ret = SQLConnect( ODconn, (SQLCHAR*) name, SQL_NTS,
           (SQLCHAR*) connection.user, SQL_NTS, (SQLCHAR*) connection.password, SQL_NTS );    
    if ((ret != SQL_SUCCESS) && ( ret != SQL_SUCCESS_WITH_INFO) )
    {
        SQLGetDiagRec( SQL_HANDLE_DBC, ODconn, 1, NULL, &err, msg, sizeof(msg), NULL );
	snprintf( emsg, sizeof(emsg), "SQLConnect():\n%s (%d)\n", msg, err );
	report_error( emsg );
	return DB_FAILED;
    }    

    return DB_OK;
}

int
db_driver_close_database()
{
    SQLDisconnect( ODconn );  
    close_connection();
    return DB_OK;
}
