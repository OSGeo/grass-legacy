#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include "globals.h"

$include sqltypes;

/* NAME: db_driver_init
 * INPUT: none
 * OUTPUT: none
 * PROCESSING:
 */
int
db_driver_init (argc, argv) char *argv[];
{
    return DB_OK;
}

/* NAME: db_driver_finish
 * INPUT: none
 * OUTPUT: none
 * PROCESSING:
 */
int
db_driver_finish()
{
    return DB_OK;
}

/* NAME: db_driver_open_database
 * INPUT: dbHandle
 * OUTPUT: error code
 * PROCESSING: DATABASE database name
 */
int
db_driver_open_database(handle)
    dbHandle *handle;
{
    $char *databaseName;
    char *name;
    char *path;

    name = db_get_handle_dbname(handle);
    path = db_get_handle_dbpath(handle);
    databaseName = db_malloc (strlen(path) + strlen(name) + 2);
    if (databaseName == NULL)
	return DB_FAILED;
    if (*path && informix_engine_is_standard())
    {
	sprintf(databaseName, "%s/%s", path, name);
    }
    else
    {
	sprintf(databaseName, "%s", name);
    }

/* open the database */
    $DATABASE $databaseName;
    free(databaseName);

/* if error, return failure */
    if(sql_error(NULL))
    {
	return DB_FAILED;
    }

/* success */
    return DB_OK;
}

/* NAME: db_driver_close_database
 * INPUT: dbMessage
 * OUTPUT: error code
 * PROCESSING: CLOSE DATABASE
 */
int
db_driver_close_database()
{
/* close currently open database */
    $CLOSE DATABASE;
/* if error do error return */
    if(sql_error(NULL))
    {
	return DB_FAILED;
    }

/* return success */
    return DB_OK;
}

