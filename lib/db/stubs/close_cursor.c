#include "dbmi.h"

int
db_driver_close_cursor (cursor)
    dbCursor *cursor;
{
    db_procedure_not_implemented("db_close_cursor");
    return DB_FAILED;
}
