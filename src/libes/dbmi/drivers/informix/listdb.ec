#include <dbmi.h>
#include "globals.h"

static char *rfind();

db_driver_list_databases (dbpath, npaths, dblist, dbcount)
    dbString *dbpath;
    int npaths;
    dbHandle **dblist;
    int *dbcount;
{
    dbDirent *dirent;
    char *path;
    int   ndirs, nfiles;
    char *name;
    char *cp;
    int i,j;
    dbHandle *list;
    int count;

    *dblist  = NULL;
    *dbcount = 0;

    list = NULL;
    count = 0;
    dirent = NULL;

    /* if Informix On-line, don't know how to list databases, so */
    /*  return null list with zero count, set above */
    if (!informix_engine_is_standard()) {
      return DB_OK;
    }

/* get DBPATH */
    if (npaths > 0)
	path = build_dbpath(dbpath, ndirs = npaths);
    else
	path = get_dbpath (&ndirs);

    for(i=0; i<ndirs;i++, path = path + strlen(path) + 1)
    {	
	dirent = db_dirent (path, &nfiles);
	if (dirent == NULL) continue;
	for (j = 0; j<nfiles;j++)
	{
	/* databases are directories */
	    if (!dirent[j].isdir) continue; 
	/* that have .dbs extension */
	    name = db_get_string(&dirent[j].name);
	    cp = rfind (name, ".dbs");
	    if (cp == NULL) continue;	
	/* remove the .dbs extension */
	    *cp = 0;
	/* make sure name isn't an empty string */
	    if (*name == 0) continue;
	/* add the name, path to the list */
	    list = (dbHandle *) db_realloc (list, (count + 1) * sizeof(*list));
	    if (list  == NULL)
		goto die;
	    db_init_handle (&list[count]);
	    if(db_set_handle (&list[count], name, path) != DB_OK)
		goto die;
	    count++;
	}
	db_free_dirent_array(dirent, nfiles);
    }
    *dblist  = list;
    *dbcount = count;
    return DB_OK;
die:
    db_free_dirent_array (dirent, nfiles);
    db_free_handle_array (list, count);
    return db_get_error_code();
}

static char *
rfind (string, tail)
    char *string;
    char *tail;
{
    int tail_len;
    int string_len;

    tail_len = strlen(tail);
    string_len = strlen(string);
    if (string_len < tail_len)
	return ( (char *) NULL);
    string += (string_len - tail_len);
    if (strcmp (tail, string) == 0)
	return string;
    return ( (char *) NULL);
}
