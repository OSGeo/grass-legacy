#include "datetime.h"
#include <stdio.h>

main(argc, argv) char *argv[];
{
    DateTime a, b, result;
    char buf[1024];

    if (argc != 3)
    {
	fprintf (stderr, "Usage: %s \"datetime\" \"datetime\"\n",argv[0]);
	exit(1);
    }

    if (datetime_scan(&a, argv[1]) != 0)
    {
	fprintf (stderr, "%s: (%d) %s\n",
	    argv[1], datetime_error_code(), datetime_error_msg());
	exit(1);
    }

    if (datetime_scan(&b, argv[2]) != 0)
    {
	fprintf (stderr, "%s: (%d) %s\n",
	    argv[2], datetime_error_code(), datetime_error_msg());
	exit(1);
    }
    if (!datetime_is_absolute(&a))
    {
	fprintf (stderr, "[%s] - must be an absolute date\n", argv[1]);
	exit(1);
    }
    if (!datetime_is_absolute(&b))
    {
	fprintf (stderr, "[%s] - must be an absolute date\n", argv[2]);
	exit(1);
    }

    if (datetime_is_same (&a, &b)){
	fprintf (stderr, "[%s] and [%s] are the same\n", argv[1], argv[2]);
    }
    if (datetime_difference (&a, &b, &result) != 0)
    {
	fprintf (stderr, "ERROR: (%d) %s\n",
	    datetime_error_code(), datetime_error_msg());
	exit(1);
    }

    datetime_format (&result, buf);
    fprintf (stdout,"%s\n", buf);

    exit(0);
}
