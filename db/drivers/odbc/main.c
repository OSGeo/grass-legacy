#define MAIN
#include <dbmi.h>
#include <odbc/sql.h>
#include <odbc/sqlext.h>
#include <odbc/sqltypes.h>
#include "globals.h"

main(argc, argv) char *argv[];
{
    exit (db_driver (argc, argv));
}
