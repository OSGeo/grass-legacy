#include "dbmi.h"

int
db_driver_update (dbCursor *cursor)
{
    db_procedure_not_implemented("db_update");
    return DB_FAILED;
}
