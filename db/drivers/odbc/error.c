#include <dbmi.h>
#include "gis.h"
#include "odbc.h"
#include "globals.h"
#include <stdio.h>

void
report_error (err)
    char *err;
{
    char *msg = NULL;

    G_asprintf (&msg, "DBMI-ODBC driver error: %s", err);
    db_error (msg);
    G_free(msg);
}

