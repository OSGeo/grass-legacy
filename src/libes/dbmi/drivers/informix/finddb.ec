#include <dbmi.h>
#include "globals.h"

static int build_fullname();

db_driver_find_database (handle, found)
    dbHandle *handle;
    int *found;
{
    dbString fullname;
    char *dbname;
    char *dbpath;
    char *path;
    int i,ndirs;

    /* if Informix On-line, don't know how to list databases, so */
    /*  return not found */
    if (!informix_engine_is_standard()) {
      *found = 0;
      return DB_OK;
    }

    *found = 0;

    dbname = db_get_handle_dbname(handle);
    dbpath = db_get_handle_dbpath(handle);
    if (*dbpath)
    {
	if(!build_fullname (dbname, dbpath, &fullname))
	    goto die;
	if (db_isdir (db_get_string (&fullname)) == DB_OK)
	    *found = 1;
    }
    else
    {
/* get DBPATH */
	path = get_dbpath (&ndirs);

	for(i=0; i<ndirs;i++)
	{	
	    if(!build_fullname (dbname, path, &fullname))
		goto die;
	    if (db_isdir (db_get_string (&fullname)) == DB_OK)
		break;
	    path = path + strlen(path) + 1;
	}
	if (i < ndirs)
	{
	    if(db_set_handle (handle, dbname, path) != DB_OK)
		goto die;
	    *found = 1;
	}
    }
    db_free_string (&fullname);
    return DB_OK;
die:
    db_free_string (&fullname);
    return DB_FAILED;
}

static int
build_fullname (dbname, dbpath, fullname)
    char *dbname;
    char *dbpath;
    dbString *fullname;
{

    if(db_set_string    (fullname, dbpath) 		   != DB_OK) return 0;
    if(db_append_string (fullname, "/")                    != DB_OK) return 0;
    if(db_append_string (fullname, dbname)                 != DB_OK) return 0;
    if(db_append_string (fullname, ".dbs")                 != DB_OK) return 0;

    return 1;
}
