/* %W% %G% */

#include "dma.h"
seek_out (offset)
    long offset;
{
    if (lseek (outfd, offset, 0) < 0)
    {
	char msg[100];

	sprintf (msg, "%s: can't seek", outname);
	error (msg,1);
	exit(1);
    }
}

write_out (buf, n)
    char *buf;
{
    if (write (outfd, buf, n) != n)
    {
	char msg[100];

	sprintf (msg, "%s: can't write", outname);
	error (msg,1);
	exit(1);
    }
}
