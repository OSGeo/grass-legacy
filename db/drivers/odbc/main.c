#define MAIN
#include <dbmi.h>
#include "odbc.h"
#include "globals.h"
#include "dbdriver.h"

main(argc, argv) char *argv[];
{
	init_dbdriver();
	exit (db_driver (argc, argv));
}
