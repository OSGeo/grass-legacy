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
 * A pointer is used. This can be a crosshair, pointer, or cursor. 
 * It starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 */

#include <gl.h>
#include <device.h>
#include <stdio.h>


extern int SCREEN_BOTTOM;


Get_location_with_pointer(wx, wy, button)
	int *wx, *wy ;    /* new x,y coordinate       */
	int *button ;     /* button pressed to return */
{
    long val;
    long x, y, xorg, yorg;
    int cur_x, cur_y;


    /* note that LEFTMOUSE == MOUSE3 */

    /* wait till they release button from last call */
    while (getbutton (MOUSE1) || getbutton (MOUSE2) || getbutton (MOUSE3))
	;

    qdevice (LEFTMOUSE);
    qdevice (MIDDLEMOUSE);
    qdevice (RIGHTMOUSE);
    qreset ();
    
    cur_x = cur_y = -1;

    *button = 0;
    while (1)
    {
	if (qtest ())
	{
	    switch (qread (&val)) {
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
		    cur_x = val;
		    break;
		case MOUSEY:
		    cur_y = val;
		    break;
	    }
	}
	if (*button)
	{
	    x = getvaluator (MOUSEX);
	    y = getvaluator (MOUSEY);
	    break;
	}
    }

    unqdevice (LEFTMOUSE);
    unqdevice (MIDDLEMOUSE);
    unqdevice (RIGHTMOUSE);
    qreset ();

    getorigin (&xorg, &yorg);

    *wx = x - xorg;
    if (*wx < 0)
	*wx = 0;

    *wy =  SCREEN_BOTTOM -  (y - yorg);
    if (*wy < 0)
	*wy = 0;

}
