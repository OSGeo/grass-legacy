/* %W% %G% */

#include "dma.h"

zero_out(nbytes)
    long nbytes ;
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
}
