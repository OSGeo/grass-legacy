#include "P.h"
Pdata (buf, n)
    unsigned char *buf;
{
    int col;
    int cyan, yellow, majenta ;
    unsigned char value;

    col = 0;
    while (n-- > 0)
    {
	value = *buf++;

	cym (value, &cyan, &yellow, &majenta);
	dither (col++, cyan, yellow, majenta);
    }
    ras_row++;
    if (ras_row == 3)
	flush_raster();
}
