#include "dbmi.h"

int
db_driver_list_databases (path, npaths, handles, num)
    dbString *path;
    int npaths;
    dbHandle **handles;
    int *num;
{
    db_procedure_not_implemented("db_list_databases");
    return DB_FAILED;
}
