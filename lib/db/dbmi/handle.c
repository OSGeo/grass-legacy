#include "dbmi.h"

void
db_init_handle (handle)
    dbHandle *handle;
{
    db_init_string (&handle->dbName);
    db_init_string (&handle->dbPath);
}

db_set_handle (handle, dbName, dbPath)
    dbHandle *handle;
    char *dbName;
    char *dbPath;
{
    int stat;

    stat = db_set_string (&handle->dbName, dbName);
    if (stat != DB_OK)
	return stat;
    stat = db_set_string (&handle->dbPath, dbPath);
    return stat;
}

char *
db_get_handle_dbname(handle)
    dbHandle *handle;
{
    return db_get_string (&handle->dbName);
}

char *
db_get_handle_dbpath(handle)
    dbHandle *handle;
{
    return db_get_string (&handle->dbPath);
}

void
db_free_handle(handle)
    dbHandle *handle;
{
    db_free_string (&handle->dbName);
    db_free_string (&handle->dbPath);
}

void
db_free_handle_array (handle, count)
    dbHandle *handle;
    int count;
{
    int i;

    if (handle)
    {
	for (i = 0; i < count; i++)
	    db_free_handle(&handle[i]);
	free(handle);
    }
}

dbHandle *
db_alloc_handle_array (count)
    int count;
{
    int i;
    dbHandle *handle;

    handle = (dbHandle *) db_calloc (count, sizeof(dbHandle));
    if (handle)
	for (i = 0; i < count; i++)
	    db_init_handle (&handle[i]);
    return handle;
}
