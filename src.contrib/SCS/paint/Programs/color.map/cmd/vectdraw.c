#include <stdio.h>
#include "graphics.h"

#include "Vect.h"


static struct line_pnts *line_p;	/* digit libes */
static int first = 1;

vectdraw (Map)
    struct Map_info *Map;
{
    int type;

/* initialization required by dig library interface */
    if (first)
    {
	line_p = Vect_new_line_struct ();
	first = 0;
    }


    while ((type = Vect_read_next_line (Map, line_p)) >= 0)
	plot_lines (line_p->n_points, line_p->x, line_p->y, type);
}

static
plot_lines (nc, east, north, type)
    double *east, *north;
{
    double e1, e2;
    double n1, n2;
    int x, y;
    int n;

    if (nc < 1)
        return 0;

    e2 = *east++;
    n2 = *north++;
    if (vect_type_is_dot(type))
    {
	G_plot_where_xy (e2, n2, &x, &y);
        diamond (x, y);
        return 0;
    }

    for (n = 1; n < nc; n++)
    {
        e1 = e2;
        n1 = n2;
	e2 = *east++;
	n2 = *north++;
	G_plot_line (e1, n1, e2, n2);
    }
    return 0;
}
