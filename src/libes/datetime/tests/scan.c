#include "datetime.h"
#include <stdio.h>

main(argc, argv) char *argv[];
{
    DateTime dt;
    int i;
    char result[1024];
    char input[1024];

    *input = 0;
    for (i=1; i < argc; i++)
    {
	if (*input) strcat (input, " ");
	strcat (input, argv[i]);
    }
    if (datetime_scan(&dt, input) != 0)
    {
	fprintf (stderr, "ERROR(%d): %s\n",
	    datetime_error_code(), datetime_error_msg());
    }
    else
    {
	datetime_format (&dt, result);
	fprintf (stdout,"%s\n", result);
    }
    exit(0);
}
