#include <dbmi.h>
#include "globals.h"

/* NAME: db_driver_create_database 
 * INPUT:
 * OUTPUT:
 * PROCESSING:
 */
db_driver_create_database (handle)
dbHandle *handle;
{
  char *dbname, *dbpath;
  $char cmd[200];

  dbname = db_get_handle_dbname(handle);
  dbpath = db_get_handle_dbpath(handle);

  /* concatenating the path and the name is okay, since if the user
   * specified more than one directory in the path, the CREATE DATABASE
   * will fail
   */
  if (*dbpath)
  {
    if (informix_engine_is_standard())
	sprintf(cmd, "CREATE DATABASE \"%s/%s\"", dbpath, dbname);
    else
	sprintf(cmd, "CREATE DATABASE \"%s\" IN \"%s\"", dbname, dbpath);
  }
  else
    sprintf(cmd, "CREATE DATABASE \"%s\"", dbname);

  $EXECUTE IMMEDIATE $cmd;
  if (sql_error(NULL))
    return DB_FAILED;

  return DB_OK;
}

/* NAME: db_driver_delete_database 
 * INPUT:
 * OUTPUT:
 * PROCESSING:
 */
db_driver_delete_database (handle)
dbHandle *handle;
{
  char *dbname, *dbpath;
  $char cmd[200];

  dbname = db_get_handle_dbname(handle);
  dbpath = db_get_handle_dbpath(handle);

  /* concatenating the path and the name is okay, since if the user
   * specified more than one directory in the path, the DROP DATABASE
   * will fail
   */
  if (*dbpath && informix_engine_is_standard())
    sprintf(cmd, "DROP DATABASE \"%s/%s\"", dbpath, dbname);
  else
    sprintf(cmd, "DROP DATABASE \"%s\"", dbname);

  $EXECUTE IMMEDIATE $cmd;
  if (sql_error(NULL))
    return DB_FAILED;

  return DB_OK;
}
