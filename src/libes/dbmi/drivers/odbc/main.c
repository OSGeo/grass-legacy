#define MAIN
#include <dbmi.h>
#include "odbc.h"
#include "globals.h"

main(argc, argv) char *argv[];
{
    exit (db_driver (argc, argv));
}
