#include "datetime.h"
#include <stdio.h>

main(argc, argv) char *argv[];
{
    DateTime dt, incr;
    char buf[1024];

    if (argc != 4)
    {
	fprintf (stderr, "Usage: %s \"datetime\" [+|-] \"datetime\"\n",argv[0]);
	exit(1);
    }

    if (datetime_scan(&dt, argv[1]) != 0)
    {
	fprintf (stderr, "%s: (%d) %s\n",
	    argv[1], datetime_error_code(), datetime_error_msg());
	exit(1);
    }

    if (datetime_scan(&incr, argv[3]) != 0)
    {
	fprintf (stderr, "%s: (%d) %s\n",
	    argv[3], datetime_error_code(), datetime_error_msg());
	exit(1);
    }

    if (strcmp(argv[2],"-") == 0)
	datetime_set_negative(&incr);
    else if (strcmp(argv[2],"+") != 0)
    {
	fprintf (stderr, "%s: must be + or -\n", argv[2]);
	exit(1);
    }
    if (datetime_increment (&dt, &incr) != 0)
    {
	fprintf (stderr, "ERROR: (%d) %s\n",
	    datetime_error_code(), datetime_error_msg());
	exit(1);
    }

    datetime_format (&dt, buf);
    fprintf (stdout,"%s\n", buf);

    exit(0);
}
