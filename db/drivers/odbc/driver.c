#include <dbmi.h>
#include <odbc/sql.h>
#include <odbc/sqlext.h>
#include <odbc/sqltypes.h>
#include "globals.h"

int
db_driver_init (argc, argv) char *argv[];
{
    return DB_OK;
}

int
db_driver_finish()
{
    return DB_OK;
}
