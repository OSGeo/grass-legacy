#include "dbmi.h"

db_driver_delete (cursor)
    dbCursor *cursor;
{
    db_procedure_not_implemented("db_delete");
    return DB_FAILED;
}
