#include "dbmi.h"
#include "macros.h"

int
db_d_version()
{
/* no arg(s) */

/* send the return code */
    DB_SEND_SUCCESS();

/* send version */
    DB_SEND_C_STRING (DB_VERSION);
    return DB_OK;
}
