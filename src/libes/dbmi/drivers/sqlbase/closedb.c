#include "globals.h"

int
db_driver_close_database()
{
    close_database();
    return DB_OK;
}

void
close_database()
{
/* disconnect from the database, if connected */
    disconnect_from_database(&database_cursor);
    if (database_name)
    {
	free(database_name);
	database_name = NULL;
    }

/* disconnect from the server, if connected */
    disconnect_from_server(&server_handle);
    if (server_name)
    {
	free(server_name);
	server_name = NULL;
    }
}

void
disconnect_from_database (cur)
    SQLTCUR *cur;
{
    if(*cur)
    {
	sqldis(*cur);
	*cur = 0;
    }
}
