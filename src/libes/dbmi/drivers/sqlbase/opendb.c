#include "globals.h"

void make_database_name();

db_driver_open_database (handle)
    dbHandle *handle;
{
    char *name;
    char *server;

    /* make sure there isn't a database open */
    /* not really necessary since the DBMI/API won't allow this to happen */
    close_database();

    name = db_get_handle_dbname(handle);
    server = db_get_handle_dbpath(handle);

    if (connect_to_server (server, &server_handle) != DB_OK)
	return DB_FAILED;

    if(open_database (name, &database_cursor) != DB_OK)
	return DB_FAILED;
    database_name = db_store (name);
    return DB_OK;
}

open_database (name, cursor)
    char *name;
    SQLTCUR *cursor;
{
    char msg[1024];
    char fullname[1024];
    SQLTRCD rcd;

    make_database_name (fullname, name);
    if(rcd = sqlcnc(cursor, fullname, NULL_TERMINATED))
    {
	*cursor = 0;
	sprintf (msg, "Unable to connect to database (%s)", name);
	report_error (rcd, msg);
	return DB_FAILED;
    }
    return DB_OK;
}

database_is_open()
{
    return (database_cursor != 0);
}

check_for_open_database()
{
    int ok;

    if(!(ok = database_is_open()))
	db_error ("No open database");
    return ok;
}
