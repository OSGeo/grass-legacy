/*
 * Tektroniks driver for startchart.c mainline
 */

#include <stdio.h>
#include "starchart.h"

/*
 * Chart parameters (limiting magnitude and window x,y,w,h)
 */

mapblock thumbnail =	{ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
			3.2, 420, 35, 480, 195, 0.0 };

mapblock master =	{ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
			11.0, 20, 265, 880, 500, 0.0 };

/*
 * Generic Star Drawing Stuff
 */

vecopen()
   {
   tekclear();
   }

vecclose()
    {
    tekmove(0,0);
    tekalpha();
    fflush(stdout);
    }

vecsize(points)
    int points;
    {
    }

vecmove(x, y)
    {
    tekmove(x, y);
    }

vecdraw(x, y)
    {
    tekdraw(x, y);
    }

vecsym(x, y, s)
    char s;
    {
    tekmove(x, y-11); /* center character strings */
    tekalpha();
    printf("%c",s);
    }

vecsyms(x, y, s)
    char *s;
    {
    tekmove(x, y-11); /* center character strings */
    tekalpha();
    printf(s);
    }

vecmovedraw(x1, y1, x2, y2)
    {
    tekmove(x1, y1);
    tekdraw(x2, y2);
    fflush(stdout);
    }

#define XSCALE 15/8
#define YSCALE 8/5
#define XSCALEI 8/15
#define YSCALEI 5/8

drawlen(x, y, dx, dy, len)
    {
    vecmovedraw((x*XSCALEI+dx)*XSCALE, (y*YSCALEI+dy)*YSCALE,
	(x*XSCALEI+dx+len-1)*XSCALE+1, (y*YSCALEI+dy)*YSCALE);
    }

drawP(x, y)
    {
    drawlen(x, y, -1, -2, 3); /*   ***   */
    drawlen(x, y, -2, -1, 1); /*  *   *  */
    drawlen(x, y,  2, -1, 1);
    drawlen(x, y, -2,  0, 1); /*  * * *  */
    drawlen(x, y,  0,  0, 1);
    drawlen(x, y,  2,  0, 1);
    drawlen(x, y, -2,  1, 1); /*  *   *  */
    drawlen(x, y,  2,  1, 1);
    drawlen(x, y, -1,  2, 3); /*   ***   */
    }

drawstar(x, y, mag)
    {
    switch (mag)
	{
	case -1: draw0(x, y); break;
	case  0: draw0(x, y); break;
	case  1: draw1(x, y); break;
	case  2: draw2(x, y); break;
	case  3: draw3(x, y); break;
	case  4: draw4(x, y); break;
	case  5: draw5(x, y); break;
	case  6: draw6(x, y); break;
	case  7: draw7(x, y); break;
	case  8: draw8(x, y); break;
	case  9: draw9(x, y); break;
	default: draw9(x, y); break;
	}
    }

draw0(x, y)
    {
    drawlen(x, y,  0, -4, 1); /*     *     */
    drawlen(x, y, -2, -3, 5); /*   *****   */
    drawlen(x, y, -3, -2, 7); /*  *******  */
    drawlen(x, y, -3, -1, 7); /*  *******  */
    drawlen(x, y, -5,  0, 9); /* ********* */
    drawlen(x, y, -3,  1, 7); /*  *******  */
    drawlen(x, y, -3,  2, 7); /*  *******  */
    drawlen(x, y, -2,  3, 5); /*   *****   */
    drawlen(x, y,  0,  4, 1); /*     *     */
    }

draw1(x, y)
    {
    drawlen(x, y, -2, -3, 5); /*   *****   */
    drawlen(x, y, -3, -2, 7); /*  *******  */
    drawlen(x, y, -3, -1, 3); /*  *** ***  */
    drawlen(x, y,  1, -1, 3); /*  **   **  */
    drawlen(x, y, -3,  0, 2); /*  *** ***  */
    drawlen(x, y,  2,  0, 2); /*  *******  */
    drawlen(x, y,  1,  1, 3); /*   *****   */
    drawlen(x, y, -3,  1, 3);
    drawlen(x, y, -3,  2, 7);
    drawlen(x, y, -2,  3, 5);
    }

draw2(x, y)
    {
    drawlen(x, y, -1, -3, 2); /*   **    */
    drawlen(x, y, -2, -2, 5); /*  *****  */
    drawlen(x, y, -2, -1, 6); /*  ****** */
    drawlen(x, y, -3,  0, 6); /* ******* */
    drawlen(x, y, -3,  1, 6); /* ******  */
    drawlen(x, y, -1,  2, 4); /*   ****  */
    drawlen(x, y,  0,  3, 2); /*    **   */
	}

draw3(x, y)
    {
    drawlen(x, y,  0, -3, 1); /*    *    */
    drawlen(x, y, -1, -2, 3); /*   ***   */
    drawlen(x, y, -2, -1, 5); /*  *****  */
    drawlen(x, y, -3,  0, 7); /* ******* */
    drawlen(x, y, -2,  1, 5); /*  *****  */
    drawlen(x, y, -1,  2, 3); /*   ***   */
    drawlen(x, y,  0,  3, 1); /*    *    */
	}

draw4(x, y)
    {
    drawlen(x, y, -1, -2, 3); /*   ***   */
    drawlen(x, y, -2, -1, 5); /*  *****  */
    drawlen(x, y, -2,  0, 5); /*  *****  */
    drawlen(x, y, -2,  1, 5); /*  *****  */
    drawlen(x, y, -1,  2, 3); /*   ***   */
    }

draw5(x, y)
    {
    drawlen(x, y,  0, -2, 2); /*    **   */
    drawlen(x, y, -2, -1, 3); /*  ****   */
    drawlen(x, y, -2,  0, 5); /*  *****  */
    drawlen(x, y, -1,  1, 4); /*   ****  */
    drawlen(x, y, -1,  2, 1); /*   **    */
    }

draw6(x, y)
    {
    drawlen(x, y,  0, -2, 1); /*    *    */
    drawlen(x, y, -1, -1, 3); /*   ***   */
    drawlen(x, y, -2,  0, 5); /*  *****  */
    drawlen(x, y, -1,  1, 3); /*   ***   */
    drawlen(x, y,  0,  2, 1); /*    *    */
    }

draw7(x, y)
    {
    drawlen(x, y, -1, -1, 3); /*   ***   */
    drawlen(x, y, -1,  0, 3); /*   ***   */
    drawlen(x, y, -1,  1, 3); /*   ***   */
    }

draw8(x, y)
    {
    drawlen(x, y,  0, -1, 1); /*    *    */
    drawlen(x, y, -1,  0, 3); /*   ***   */
    drawlen(x, y,  0,  1, 1); /*    *    */
    }

draw9(x, y)
    {
    drawlen(x, y,  0,  0, 1); /*    *    */
    }

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
