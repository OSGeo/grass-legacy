#include "globals.h"

db_driver_find_database (handle, found)
    dbHandle *handle;
    int *found;
{
    char buf[1024], *server, *name;
    char *fullname;
    char dbname[256], username[256], passwd[256];
    int len;

    *found = 0;

    server = db_get_handle_dbpath(handle);
    if (server == NULL || *server == 0)
	server = get_sqlbase_server();

/* get the database name */
    fullname = db_get_handle_dbname(handle);
    decompose_database_name (fullname, dbname, username, passwd);

/* get a list of database names */
    if(sqldbn (server, buf, sizeof(buf)))
	return DB_OK; /* can't get a list. silently return not found */
    for (name = buf; len = strlen(name); name += len + 1)
    {
	if(db_nocase_compare(name, dbname))
	{
	    if(db_set_handle (handle, fullname, server) != DB_OK)
		return DB_FAILED;
	    *found = 1;
	    break;
	}
    }

    return DB_OK;
}
