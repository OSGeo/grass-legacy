#define MAIN
#include <stdlib.h>
#include "dbmi.h"
#include "globals.h"
#include "dbdriver.h"

int main(argc, argv) char *argv[];
{
	init_dbdriver();
	exit (db_driver (argc, argv));
}
