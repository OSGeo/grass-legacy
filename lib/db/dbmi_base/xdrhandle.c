#include "dbmi.h"
#include "macros.h"

db__send_handle (handle)
    dbHandle *handle;
{
    DB_SEND_STRING (&handle->dbName);
    DB_SEND_STRING (&handle->dbSchema);

    return DB_OK;
}

db__recv_handle (handle)
    dbHandle *handle;
{
    DB_RECV_STRING (&handle->dbName);
    DB_RECV_STRING (&handle->dbSchema);

    return DB_OK;
}
