#include "P.h"
Pdata (buf, n)
    unsigned char *buf;
{
    unsigned char white;
    if (n > ncols) n = ncols;

    write_rasterfile(buf,n);

    white = WHITE;
    while (n++ < ncols)
	write_rasterfile (&white, 1);

    nrows++;
}
