#include "dbmi.h"
#include "globals.h"

main(argc, argv) char *argv[];
{
  if (getenv("WHICH_ENGINE"))
	fprintf(stderr, "ENGINE=%s\n", informix_engine());

  exit (db_driver (argc, argv));
}
