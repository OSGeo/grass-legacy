#include "dbmi.h"

int
db_driver_drop_table (name)
    dbString *name;
{
    db_procedure_not_implemented("db_drop_table");
    return DB_FAILED;
}
