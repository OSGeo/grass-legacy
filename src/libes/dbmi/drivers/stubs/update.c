#include "dbmi.h"

int
db_driver_update (cursor)
    dbCursor *cursor;
{
    db_procedure_not_implemented("db_update");
    return DB_FAILED;
}
