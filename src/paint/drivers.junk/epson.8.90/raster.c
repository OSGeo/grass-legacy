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
    if (ras_row)
	print_raster();
    ras_row = 0;

    clear (YELLOW,  ncols);
    clear (CYAN,    ncols);
    clear (MAGENTA, ncols);
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

/* move down 8/180 inch */
    esc ("J");
    Poutc ((char)12);
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
/* set absolute position to left margin */
    Poutc('\r');

/* select 90 dots/in */
    esc("*");
    Poutc((char)6);
    Poutc((unsigned char)(ncols%256));
    Poutc((unsigned char)(ncols/256));

/* send */

    Pout (color, ncols);

/* set absolute position to left margin */
    Poutc('\r');
}
