/* Function show_icon
**
** This function reads the icon file and displays the icon.
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_icon.h"

show_icon()
{
    double x, y, f;

    /* compute conversion factor */
    f = (double)(icon.x_max - icon.x_min);

    /* read the coordinates from the icon file */
    icon.points = 0;
    while (fscanf(icon.fp, "%lf %lf", &x, &y) == 2)
    {
	icon.xp[icon.points] = icon.cx + (int)(x * f);
	icon.yp[icon.points] = icon.cy - (int)(y * f);
	icon.points++;
    }

    /* close the icon file */
    fclose(icon.fp);

    /* draw icon in white */
    R_open_driver();
    R_standard_color(D_translate_color("white"));

    /* draw the icon */
    R_polygon_abs(icon.xp, icon.yp, icon.points);
    R_close_driver();
}
