#include "globals.h"

db_driver_list_databases (dbpath, npaths, dblist, dbcount)
    dbString *dbpath;
    int npaths;
    dbHandle **dblist;
    int *dbcount;
{
    int pass;
    char *server;
    int count, n;
    dbHandle *list;

    *dblist  = NULL;
    *dbcount = 0;

/* for each server, get a list of database names */
    list = NULL;
    for (pass = 0; pass < 2; pass++)
    {
	count = 0;
	for (n = 0; server = get_server_name_from_list (n, dbpath, npaths); n++)
	{
	    if(list_dbnames(server, list, &count) != DB_OK)
		goto fail;
	}
	if(n == 0)
	{
	    server = NULL;
	    if(list_dbnames(server, list, &count) != DB_OK)
		goto fail;
	}
	if(pass == 0)
	{
	    list = db_alloc_handle_array (count);
	    if(list == NULL)
		goto fail;
	}
    }

    *dblist = list;
    *dbcount = count;
    return DB_OK;
fail:
    db_free_handle_array (list, count);
    return DB_FAILED;
}

list_dbnames (server, list, count)
    char *server;
    dbHandle *list;
    int *count;
{
    char buf[1024];
    char *name;
    int len;
    int i;

    if(sqldbn (server, buf, sizeof(buf)))
	return DB_OK; /* can't get a list - silently return OK */

    for (name = buf; len = strlen(name); name += len + 1)
    {
	i = (*count)++;
	if(list)
	{
	    db_init_handle (&list[i]);
	    if(db_set_handle (&list[i], name, server) != DB_OK)
		return DB_FAILED;
	}
    }

    return DB_OK;
}
