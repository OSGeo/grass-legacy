#include "dbmi.h"

int
db_driver_list_tables (dbString **names, int *count, int system)
{
    db_procedure_not_implemented("db_list_tables");
    return DB_FAILED;
}
