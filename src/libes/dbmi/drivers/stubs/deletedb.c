#include "dbmi.h"

int
db_driver_delete_database (handle)
    dbHandle *handle;
{
    db_procedure_not_implemented("db_delete_database");
    return DB_FAILED;
}
