#include <dbmi.h>
#include "odbc.h"
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
