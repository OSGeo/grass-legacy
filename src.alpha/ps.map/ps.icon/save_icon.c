/* Function save_icon
**
** This function writes the coordinates to the icon file.
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_icon.h"

save_icon()
{
    int i;
    double x, y, f;

    /* write the title */
    fprintf(icon.fp, "%s\n", icon.title);

    /* compute conversion factor */
    f = 1.0 / ((double)(icon.x_max - icon.x_min));

    /* write the coordinates to icon file */
    for (i = 0; i < icon.points; i++)
    {
	x = (icon.xp[i] - icon.cx) * f;
	y = (icon.cy - icon.yp[i]) * f;
	fprintf(icon.fp, "%.4f %.4f\n", x, y);
    }

    /* close the icon file */
    fclose(icon.fp);
}
