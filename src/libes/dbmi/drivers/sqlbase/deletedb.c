#include "globals.h"

/* NAME: db_driver_create_database 
 * INPUT:
 * OUTPUT:
 * PROCESSING:
 */
db_driver_delete_database (handle)
    dbHandle *handle;
{
    SQLTRCD rcd;	/* return code */
    SQLTSVH svh;	/* server handle for this new database */
    char *dbname, *server;
    char buf[256];

    svh = 0;

    dbname = db_get_handle_dbname(handle);
    server = db_get_handle_dbpath(handle);
    if (*server == 0) server = NULL; /* use default server */

    if(connect_to_server (server, &svh) != DB_OK)
	return DB_FAILED;

/* delete the database */
    if (rcd = sqldel (svh, dbname, NULL_TERMINATED))
    {
	sprintf (buf, "Unable to delete database (%s)", dbname);
	report_error (rcd, buf);
	goto fail;
    }

    disconnect_from_server(&svh);
    return DB_OK;
fail:
    disconnect_from_server(&svh);
    return DB_FAILED;
}
