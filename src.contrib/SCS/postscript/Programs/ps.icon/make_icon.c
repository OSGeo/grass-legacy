/* Function make_icon
**
** This function lets the user make an icon with the mouse.
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_icon.h"

make_icon()
{
    int x, y, fx, fy, n, button, white;
    int black;

    R_open_driver();

    /* instruct user */
    G_clear_screen();
    printf("\n\n\n\n\n\n                  ");
    printf("Use left mouse button to select first point...");
    fflush(stdout);

    /* draw icon in white */
    white = D_translate_color("white");
    black = D_translate_color("black");

    /* get first point */
    icon.points = 0;
    button = 0;
    x = (icon.cx + icon.x_min) / 2;
    y = (icon.cy + icon.y_max) / 2;
    while (button != 1) 
    {
	R_get_location_with_pointer(&x, &y, &button);
	if      (x < icon.x_min) x = icon.x_min;
	else if (x > icon.x_max) x = icon.x_max;
	if      (y < icon.y_min) y = icon.y_min;
	else if (y > icon.y_max) y = icon.y_max;
    }
    icon.xp[icon.points] = fx = x;
    icon.yp[icon.points] = fy = y;
    icon.points++;

    /* instruct user */
    G_clear_screen();
    printf("\n\n\n\n\n\n          ");
    printf("Left button selects next point, right button closes polygon...");
    fflush(stdout);

    /* get remaining points */
    while (button != 3)
    {
     	R_standard_color(black);
	R_get_location_with_line(fx, fy, &x, &y, &button);
	if      (x < icon.x_min) x = icon.x_min;
	else if (x > icon.x_max) x = icon.x_max;
	if      (y < icon.y_min) y = icon.y_min;
	else if (y > icon.y_max) y = icon.y_max;
	if (button == 1)
	{
    	    R_standard_color(white);
	    R_move_abs(fx, fy);
	    R_cont_abs(x, y);
    	    icon.xp[icon.points] = fx = x;
    	    icon.yp[icon.points] = fy = y;
    	    icon.points++;
	    if (icon.points == MAX_POINTS)
	    {
		R_close_driver();
		fclose(icon.fp);
		printf("Too many points.\n");
		exit(-1);
	    }
	}
    }

    /* snap to first point */
    R_standard_color(white);
    n = icon.points - 1;
    R_move_abs(icon.xp[n], icon.yp[n]);
    R_cont_abs(icon.xp[0], icon.yp[0]);

    /* fill icon */
    R_polygon_abs(icon.xp, icon.yp, icon.points);
    R_close_driver();
}

