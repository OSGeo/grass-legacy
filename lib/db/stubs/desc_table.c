#include "dbmi.h"

db_driver_describe_table (name, table)
    dbString *name;
    dbTable *table;
{
    db_procedure_not_implemented("db_describe_table");
    return DB_FAILED;
}
