#include "dma.h"

int zero_out (long nbytes)
{
    char buf[10240];
    register char *b;
    register long n;

    n = nbytes;
    if (n > sizeof(buf))
	n = sizeof(buf);
    b = buf;
    while (n-- > 0)
	*b++ = 0;
    
    while (nbytes > 0)
    {
	n = nbytes;
	if (n > sizeof(buf))
	    n = sizeof(buf);
	write_out (buf, n);
	nbytes -= n;
    }

    return 0;
}
