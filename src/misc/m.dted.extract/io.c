#include <unistd.h>
#include "dma.h"

int seek_out (long offset)
{
    if (lseek (outfd, offset, 0) < 0)
    {
	char msg[100];

	sprintf (msg, "%s: can't seek", outname);
	error (msg,1);
	exit(1);
    }

    return 0;
}

int write_out (char *buf, int n)
{
    if (write (outfd, buf, n) != n)
    {
	char msg[100];

	sprintf (msg, "%s: can't write", outname);
	error (msg,1);
	exit(1);
    }

    return 0;
}
