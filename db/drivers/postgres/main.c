#define MAIN
#include <stdlib.h>
#include "dbmi.h"
#include "globals.h"

int main(argc, argv) char *argv[];
{
    exit (db_driver (argc, argv));
}
