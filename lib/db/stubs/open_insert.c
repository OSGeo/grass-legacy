#include "dbmi.h"

db_driver_open_insert_cursor (cursor)
    dbCursor *cursor;
{
    db_procedure_not_implemented("db_open_insert_cursor");
    return DB_FAILED;
}
