#include "dbmi.h"

int
db_driver_create_index (index)
    dbIndex *index;
{
    db_procedure_not_implemented("db_create_index");
    return DB_FAILED;
}
