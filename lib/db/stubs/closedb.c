#include "dbmi.h"

int
db_driver_close_database (void)
{
    db_procedure_not_implemented("db_close_database");
    return DB_FAILED;
}
