#include <stdio.h>
/*
 * Low Level Tektronix Plotting Routines
 */

#define	GS	035
#define	US	037
#define ESC	033
#define FF	014

static int oldHiY = 0, oldLoY = 0, oldHiX = 0;

tekplot()	/* switch to plot mode */
    {
    putchar(GS);
    putchar('@');
    oldHiY = oldLoY = oldHiX = 0;
    }

tekalpha()	/* switch to alpha mode */
    {
    putchar(US);
    fflush(stdout);
    }

tekclear()
    {
    putchar(ESC);
    putchar(FF);
    fflush(stdout);
    }

tekmove(x, y)	/* move to (x,y) */
    {
    putchar(GS);
    tekdraw(x, y);
    }

tekdraw(x, y)	/* draw to (x,y) */
    {
    int hiY, loY, hiX, loX;
    if (x < 0) x = 0;
    if (y < 0) y = 0;
    if (x > 1023) x = 1023;
    if (y > 767) y = 767;

    hiY = 0040 | (y>>5 & 037),
    loY = 0140 | (y    & 037),
    hiX = 0040 | (x>>5 & 037),
    loX = 0100 | (x    & 037);

    if (hiY != oldHiY) putchar(hiY);
    if (loY != oldLoY || hiX != oldHiX) putchar(loY);
    if (hiX != oldHiX) putchar(hiX);
    putchar(loX);

    oldHiY = hiY;
    oldLoY = loY;
    oldHiX = hiX;
    }
