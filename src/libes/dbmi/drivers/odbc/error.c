#include <dbmi.h>
#include "odbc.h"
#include "globals.h"
#include <stdio.h>

void
report_error (err)
    char *err;
{
    char msg[DB_MSG];

    snprintf (msg, sizeof(msg), "DBMI-ODBC driver error: %s", err);
    db_error (msg);
}

