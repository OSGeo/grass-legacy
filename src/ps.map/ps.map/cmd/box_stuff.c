/* box functions
**
** Author: Paul W. Carlson	March 1992
*/

#include "ps_info.h"

box_path(t, b, l, r)
double t, b, l, r;
{
    fprintf(PS.fp, "%.1lf %.1lf %.1lf %.1lf B ",  l, b, r, t);
}

box_clip(t, b, l, r)
double t, b, l, r;
{
    box_path(t, b, l, r);
    fprintf(PS.fp, "clip newpath\n");
}

box_fill(t, b, l, r, color_number)
double t, b, l, r;
int color_number;
{
    set_rgb_color(color_number);
    box_path(t, b, l, r);
    fprintf(PS.fp, "F\n");
}

box_draw(t, b, l, r)
double t, b, l, r;
{
    box_path(t, b, l, r);
    fprintf(PS.fp, "D\n");
}
