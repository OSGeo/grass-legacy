#include "P.h"

Praster ()
{
}

flush_raster()
{
    int i;
    if (ras_row)	/* output the colors to printer */
	print_raster();
    ras_row = 0;

}

static
print_raster()
{
    char buf[COLORBYTES];
    int count;
    int i;

    count = ras_row * 2;

    lseek (yellowfd, 0L, 0);
    for (i = 0; i < count; i++)
    {
	read (yellowfd, buf, COLORBYTES);
	do_color (buf);
    }
    finish_color(buf);	/* complete the yellow */
    lseek (yellowfd, 0L, 0);

    Poutc (VT);	/* end the yellow */

/* magenta */
    lseek (magentafd, 0L, 0);
    for (i = 0; i < count; i++)
    {
	read (magentafd, buf, COLORBYTES);
	do_color (buf);
    }
    finish_color(buf);	/* complete the magenta */
    lseek (magentafd, 0L, 0);

    Poutc (VT);	/* end the magenta */

/* cyan */

    lseek (cyanfd, 0L, 0);
    for (i = 0; i < count; i++)
    {
	read (cyanfd, buf, COLORBYTES);
	do_color (buf);
    }
    finish_color(buf);	/* complete the cyan */
    lseek (cyanfd, 0L, 0);

    Poutc (FF);	/* end the frame */
}

finish_color(buf)
    char *buf;
{
    int count;

/*
    count = 3321 - ras_row * 2;
fprintf (stderr, "finishing last %d rows of current color\n", count);
    zero (buf, COLORBYTES);
    while(count--)
	do_color (buf);
*/
}
