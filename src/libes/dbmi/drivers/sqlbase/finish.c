#include "globals.h"

db_driver_finish ()
{
    close_database();
    return DB_OK;
}
