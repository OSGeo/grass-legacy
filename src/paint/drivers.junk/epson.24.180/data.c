#include "P.h"
Pdata (buf, n)
    unsigned char *buf;
{
    int col;
    int cyan, yellow, magenta ;
    unsigned char value;

    col = 0;
    while (n-- > 0)
    {
	value = *buf++;

	cym (value, &cyan, &yellow, &magenta);
	dither (col++, cyan, yellow, magenta);
    }
    ras_row++;
    ras_row_parity = ras_row%2;
    ras_row_grp = ras_row/8;
    ras_row_idx = ras_row%8;
    if (ras_row == ras_nrows)
	flush_raster();
}
