/* These functions provide the graphics primitives.
**
** Author: Paul W. Carlson	March 1992
*/

#include "ps_info.h"

draw_line(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    fprintf(PS.fp, "%d %d %d %d L", x2, y2, x1, y1);
}

draw_vect(x1, y1, x2, y2, i)
double x1, y1, x2, y2;
int i;
{
    /* if first line segment... */
    if (!i) fprintf(PS.fp, "%.1lf %.1lf NM ", x1, y1);

    fprintf(PS.fp, "%.1lf %.1lf V", x2 - x1, y2 - y1);
}

set_line_width(width)
int width;
{
    fprintf(PS.fp, "%d W\n", width);
}

font_name(name)
char *name;
{
    fprintf(PS.fp, "(%s) FN\n", name);
}

set_font(size)
int size;
{
    fprintf(PS.fp, "%d SF\n", size);
}

show_text(x, y, text)
double x, y;
char *text;
{
    fprintf(PS.fp, "(%s)\n", text);
    fprintf(PS.fp, "%.1lf %.1lf MS\n", x, y);
}
