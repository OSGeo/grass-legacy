#include "dbmi.h"

int
db_driver_create_table (table)
    dbTable *table;
{
    db_procedure_not_implemented("db_create_table");
    return DB_FAILED;
}
