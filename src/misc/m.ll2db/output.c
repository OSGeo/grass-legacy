#include <stdio.h>
#include <unistd.h>

int output (char *buf)
{
    fprintf (stdout,"%s", buf);
    if (isatty(0) && !isatty(1))
	fprintf (stderr, "%s", buf);

    return 0;
}
