#include "dbmi.h"

int
db_driver_open_select_cursor (dbString *select, dbCursor *cursor, int *mode)
{
    db_procedure_not_implemented("db_open_select_cursor");
    return DB_FAILED;
}
