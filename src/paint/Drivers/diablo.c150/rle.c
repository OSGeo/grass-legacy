#include "P.h"
Prle (buf, n)
    unsigned char *buf;
{
    int col;
    int cyan, yellow, magenta ;
    unsigned char value, repeat;

    clear_colors();

    col = 0;
    while (n-- > 0)
    {
	repeat = *buf++;
	value = *buf++;

	cym (value, &cyan, &yellow, &magenta);
	while (repeat-- > 0)
	    dither (col++, cyan, yellow, magenta);
    }
    output_colors();

    ras_row++;
    if (ras_row == ras_nrows)
	flush_raster();
}
