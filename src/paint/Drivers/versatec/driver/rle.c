#include "P.h"
Prle (buf, n)
    unsigned char *buf;
{
    unsigned char value, repeat;
    unsigned char *d;
    int count;

    d = data_buf;
    count = 0;
    while (n-- > 0)
    {
	repeat = *buf++;
	value = *buf++;
	if (count + repeat > ncols) break;
	count += repeat;
	while (repeat-- > 0)
	    *d++ = value;
    }
    while (count++ < ncols)
	*d++ = WHITE;
    write_rasterfile (data_buf, ncols);
    nrows++;
}
