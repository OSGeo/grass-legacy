#include "dbmi.h"

db_driver_insert (cursor)
    dbCursor *cursor;
{
    db_procedure_not_implemented("db_insert");
    return DB_FAILED;
}
