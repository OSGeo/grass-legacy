#include <dbmi.h>
#include "globals.h"
#include "proto.h"

int
db_driver_init (argc, argv) char *argv[];
{
    init_error();
    return DB_OK;
}

int
db_driver_finish()
{
    return DB_OK;
}
