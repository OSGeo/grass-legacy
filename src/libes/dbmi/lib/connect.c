#include "gis.h"
#include "dbmi.h"

int
db_set_connection( connection )
    dbConnection   *connection;
{
    if ( connection->driverName )
	G_setenv("DB_DRIVER", connection->driverName);
	
    if ( connection->driverName )
	G_setenv("DB_DRIVER", connection->driverName);

    if ( connection->databaseName )
	G_setenv("DB_DATABASE", connection->databaseName);

    if ( connection->location )
	G_setenv("DB_LOCATION", connection->location);

    if ( connection->user )
	G_setenv("DB_USER", connection->user);

    if ( connection->password )
	G_setenv("DB_PASSWORD", connection->password);

    if ( connection->keycol )
	G_setenv("DB_KEYCOL", connection->keycol);

    return DB_OK;
}

int
db_get_connection( connection )
    dbConnection   *connection;
{
    connection->driverName = G__getenv("DB_DRIVER");
    connection->databaseName = G__getenv("DB_DATABASE");    
    connection->location = G__getenv("DB_LOCATION");
    connection->user = G__getenv("DB_USER");
    connection->password = G__getenv("DB_PASSWORD");
    connection->keycol = G__getenv("DB_KEYCOL");

    return DB_OK;
}
