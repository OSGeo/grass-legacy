#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "colors.h"

u_long XD_calc_closest_colr(cmap, r, g, b)
Colormap cmap;
u_short r, g, b;
{
    /* float inc = 65535.0 / 5.0;  = 13107 */
    int r_p, g_p, b_p;

    r_p = (float)r / 13107.0 + 0.5;
    g_p = (float)g / 13107.0 + 0.5;
    b_p = (float)b / 13107.0 + 0.5;

    return((u_long)(r_p * 36 + g_p * 6 + b_p + COLOR_OFFSET));
}

