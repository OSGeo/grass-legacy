#include "P.h"
Prle (buf, n)
    unsigned char *buf;
{
    int col;
    int cyan, yellow, majenta ;
    unsigned char value, repeat;

    col = 0;
    while (n-- > 0)
    {
	repeat = *buf++;
	value = *buf++;

	cym (value, &cyan, &yellow, &majenta);
	while (repeat-- > 0)
	    dither (col++, cyan, yellow, majenta);
    }
    ras_row++;
    if (ras_row == 3)
	flush_raster();
}
