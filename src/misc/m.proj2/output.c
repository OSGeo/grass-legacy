#include <stdio.h>
#include <unistd.h>

int put_output (char *buf, int fileoutput)
{

    if (fileoutput) fprintf (stdout,"%s", buf);
    if (isatty(0) && !isatty(1) && fileoutput)
	fprintf (stderr, "%s", buf);

    return 0;
}
