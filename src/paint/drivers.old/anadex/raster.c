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
    {
	esc ("JD");	/* yellow */
	do_color (YELLOW[0], YELLOW[1]);
	if (darken)
	    do_color (YELLOW[0], YELLOW[1]);

	esc ("JC");	/* majenta */
	do_color (MAJENTA[0], MAJENTA[1]);
	if (darken)
	    do_color (MAJENTA[0], MAJENTA[1]);

	esc ("JB");	/* cyan */
	do_color (CYAN[0], CYAN[1]);
	if (darken)
	    do_color (CYAN[0], CYAN[1]);

	esc ("JA");	/* black (to be nice in case interrupted) */
	Poutc ('\n');   /* next line */

	ras_row = 0;
    }
    clear (YELLOW[0],  ncols);
    clear (YELLOW[1],  ncols);
    clear (CYAN[0],  ncols);
    clear (CYAN[1],  ncols);
    clear (MAJENTA[0],  ncols);
    clear (MAJENTA[1],  ncols);
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
do_color (buf1, buf2)
    char *buf1, *buf2;
{
    register int n;

    esc ("G");	/* 72x72 graphics */
    n = ncols;
    while (n-- > 0)
    {
	send_color (*buf1++);
	send_color (*buf2++);
    }
    esc ("P");	/* end graphics */
    Poutc (CR_WITHOUT_LF);
}

static
send_color (color)
    unsigned char color;
{
    Poutc (color | 0100);
}
