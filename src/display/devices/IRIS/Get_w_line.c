/*
 * Using mouse device, get a new screen coordinate and button number.
 * Button numbers must be the following values which correspond to the
 * following software meanings:
 *   1 - left button
 *   2 - middle button
 *   3 - right button
 *
 * This is called directly by the application programs.
 *
 * A "rubberband" line is used.  One end is fixed at the (cx, cy) coordinate.
 * The opposite end starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 */

#include <gl.h>
#include <device.h>
#include <stdio.h>

extern int SCREEN_BOTTOM;

Get_location_with_line(cx, cy, wx, wy, button)
	int cx, cy ;      /* current x,y coordinate   */
	int *wx, *wy ;    /* new x,y coordinate       */
	int *button ;     /* button pressed to return */
{
    short val;
    long x, y, xorg, yorg;
    int cur_x, cur_y;
    long prev_x, prev_y;
    long dev;
    long vert[2];


    getorigin (&xorg, &yorg);

    /* note that LEFTMOUSE == MOUSE3 */

    /* wait till they release button from last call */
    while (getbutton (MOUSE1) || getbutton (MOUSE2) || getbutton (MOUSE3))
	;

    qdevice (LEFTMOUSE);
    qdevice (MIDDLEMOUSE);
    qdevice (RIGHTMOUSE);
    qdevice (MOUSEX);
    qdevice (MOUSEY);
    qreset ();
    

    /* use overlay plane to draw mouse box */
    drawmode (OVERDRAW);

    prev_x = cx;
    prev_y = cy;

    *button = 0;
    while (1)
    {
	if (qtest ())
	{
	    switch (dev = qread (&val)) {
		case LEFTMOUSE:
		    *button = 1;
		    break;
		case MIDDLEMOUSE:
		    *button = 2;
		    break;
		case RIGHTMOUSE:
		    *button = 3;
		    break;

		case MOUSEX:
		case MOUSEY:
		    if (val)
		    {
			if (dev == MOUSEX)
			{
			    if (val == prev_x)
				continue;
			}
			else
			{
			    if (val == prev_y)
				continue;
			}
			color (0);
			clear ();
			color (2); /* white */
			if (dev == MOUSEX)
			{
			    bgnline ();
			    vert[0] = cx;
			    vert[1] =  SCREEN_BOTTOM -  cy;
			    v2i (vert);
			    vert[0] = val - xorg;
			    vert[1] = getvaluator (MOUSEY) - yorg;
			    v2i (vert);
			    endline ();

			    prev_x = val;
			}
			else
			{
			    bgnline ();
			    vert[0] = cx;
			    vert[1] =  SCREEN_BOTTOM -  cy;
			    v2i (vert);
			    vert[0] = getvaluator (MOUSEX) - xorg;
			    vert[1] = val - yorg;
			    v2i (vert);
			    endline ();

			    prev_y = val;
			}
		    }
		    continue;
		    break;
	    }
	}
	if (*button)
	{
	/* if mouse NEVER moves, will get cx,cy instead of current position */
	    x = prev_x;
	    y = prev_y;

	    break;
	}
    }

    unqdevice (LEFTMOUSE);
    unqdevice (MIDDLEMOUSE);
    unqdevice (RIGHTMOUSE);
    unqdevice (MOUSEX);
    unqdevice (MOUSEY);
    qreset ();

    /* clear the overlay plane and reset normal mode */
    color (0);
    clear ();
    drawmode (NORMALDRAW);


    *wx = x - xorg;
    if (*wx < 0)
	*wx = 0;

    *wy =  SCREEN_BOTTOM -  (y - yorg);
    if (*wy < 0)
	*wy = 0;

}

