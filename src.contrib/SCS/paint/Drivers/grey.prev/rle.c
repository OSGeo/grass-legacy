#include "P.h"
#include <stdio.h>
Prle (buf, n)
    unsigned char *buf;
{
    int *d;
    int i;
    unsigned char value, repeat;

    d = data;
    i = 0;
    while (n-- > 0)
    {
	repeat = *buf++;
	value = *buf++;
	while (repeat-- > 0)
	{
	    *d++ = value;
	    i++;
	}
    }
    send_data (i);
}
