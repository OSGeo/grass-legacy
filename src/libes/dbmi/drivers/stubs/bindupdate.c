#include "dbmi.h"

int
db_driver_bind_update (cursor)
    dbCursor *cursor;
{
    db_procedure_not_implemented("db_bind_update");
    return DB_FAILED;
}
