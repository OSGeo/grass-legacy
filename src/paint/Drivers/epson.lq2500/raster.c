#include "P.h"

Praster ()
{
    ras_row = 0;
    flush_raster();
}

end_raster ()
{
    flush_raster();
}

flush_raster()
{
    int i;
    if (ras_row)
	print_raster();
    ras_row = 0;

    i = 3;
    while (i--)
    {
	clear (YELLOW[0][i],  ncols);
	clear (YELLOW[1][i],  ncols);
	clear (CYAN[0][i],    ncols);
	clear (CYAN[1][i],    ncols);
	clear (MAGENTA[0][i], ncols);
	clear (MAGENTA[1][i], ncols);
    }
}

static
print_raster()
{
/* yellow */
    select_color (COLOR_YELLOW);
    do_color (YELLOW);

/* magenta */
    select_color (COLOR_MAGENTA);
    do_color (MAGENTA);

/* cyan */
    select_color (COLOR_CYAN);
    do_color (CYAN);

/* move down 24/180 inch */
    esc ("J");
    Poutc ((char)24);
}

static
clear (buf, n)
    register char *buf;
    register int n;
{
    while (n-- > 0)
	*buf++ = 0000;
}

static
do_color (color)
    COLOR color;
{
    int n;

/* set absolute position to left margin */
    Poutc('\r');

/* select LQ with 2*ncols */
    n = ncols*2;
    esc("*");
    Poutc((char)39);
    Poutc((unsigned char)(n%256));
    Poutc((unsigned char)(n/256));

/* send 3 bytes per strike (24 pins), in pairs per col */

    for (n = 0; n < ncols; n++)
    {
	Poutc (color[0][0][n]);
	Poutc (color[0][1][n]);
	Poutc (color[0][2][n]);
	Poutc (color[1][0][n]);
	Poutc (color[1][1][n]);
	Poutc (color[1][2][n]);
    }

/* set absolute position to left margin */
    Poutc('\r');
}
