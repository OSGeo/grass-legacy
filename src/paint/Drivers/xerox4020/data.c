#include "P.h"
Pdata (buf, n)
    unsigned char *buf;
{
    int col;
    int cyan, yellow, magenta ;
    unsigned char value;

    clear_colors();

    col = 0;
    while (n-- > 0)
    {
	value = *buf++;

	cym (value, &cyan, &yellow, &magenta);
	dither (col++, cyan, yellow, magenta);
    }
    output_colors();

    ras_row++;
    if (ras_row == ras_nrows)
	flush_raster();
}
