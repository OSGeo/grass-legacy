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
    {
	if (quality == 3)
	    flush_raster144();
	else
	    flush_raster72();
    }
    ras_row = 0;

    i = quality == 3 ? 4 : 2;
    while (i--)
    {
	clear (YELLOW[i],  ncols);
	clear (CYAN[i],    ncols);
	clear (MAGENTA[i], ncols);
    }
}

static
flush_raster72()
{
    esc ("[0q");	/* select 72 x 72 */
    esc ("P");	/* graphics mode */

    esc ("[33m");	/* yellow */
    do_color (YELLOW[0], YELLOW[1]);
    if (quality > 1)
	do_color (YELLOW[0], YELLOW[1]);

    esc ("[35m");	/* magenta */
    do_color (MAGENTA[0], MAGENTA[1]);
    if (quality > 1)
	do_color (MAGENTA[0], MAGENTA[1]);

    esc ("[36m");	/* cyan */
    do_color (CYAN[0], CYAN[1]);
    if (quality > 1)
	do_color (CYAN[0], CYAN[1]);

    esc ("[30m");	/* black (to be nice in case interrupted) */
    esc ("[60e");	/* move down 12/144 inch */
    esc ("\\");	/* end graphics */
}

static
flush_raster144()
{
    esc ("[1q");	/* select 144 x 72 */
    esc ("P");	/* graphics mode */

    esc ("[33m"); 	/* yellow */
    do_color (YELLOW[0],  YELLOW[1]);
    esc ("[5e");	/* move down 1/144 inch */
    do_color (YELLOW[2],  YELLOW[3]);
    esc ("[55e");	/* move down 11/144 inch */
    esc ("L");	/* move up! */

    esc ("[35m"); 	/* magenta */
    do_color (MAGENTA[0],  MAGENTA[1]);
    esc ("[5e");	/* move down 1/144 inch */
    do_color (MAGENTA[2],  MAGENTA[3]);
    esc ("[55e");	/* move down 11/144 inch */
    esc ("L");	/* move up! */

    esc ("[36m"); 	/* cyan */
    do_color (CYAN[0],  CYAN[1]);
    esc ("[5e");	/* move down 1/144 inch */
    do_color (CYAN[2],  CYAN[3]);
    esc ("[55e");	/* move down 11/144 inch */

    esc ("\\");	/* end graphics */
    esc ("[0q");	/* reselect 72 x 72 */

    esc ("[30m");	/* select BLACK */
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


    n = ncols;
    while (n-- > 0)
    {
	send_color (*buf1++);
	send_color (*buf2++);
    }
    Poutc ('\r');
}

static
send_color (color)
    unsigned char color;
{
    if (color < 040)
	color |= 0100;
    Poutc (color);
}
