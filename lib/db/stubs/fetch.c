#include "dbmi.h"

int
db_driver_fetch (dbCursor *cursor, int position, int *more)
{
    db_procedure_not_implemented("db_fetch");
    return DB_FAILED;
}
