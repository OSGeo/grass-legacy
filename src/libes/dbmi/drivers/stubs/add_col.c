#include "dbmi.h"

int
db_driver_add_column (tableName, column)
    dbString *tableName;
    dbColumn *column;
{
    db_procedure_not_implemented("db_add_column");
    return DB_FAILED;
}
