#include <stdio.h>

output(buf)
    char *buf;
{
    printf ("%s\n", buf);
    if (isatty(0) && !isatty(1))
	fprintf (stderr, "%s\n", buf);
}
