#include "digit.h"
#include "gis.h"

do_lines (Map, Points)
    struct Map_info *Map;
    struct line_pnts *Points;
{
    int nlines;
    int index;
    int count;
    CELL cat;

    nlines = V2_num_lines (Map);
    for (count = 0, index = 1; index <= nlines; index++)
    {
	if (Map->Line[index].type == DOT)
	    continue;
	cat = V2_line_att (Map, index);
	if (cat == 0)
	    continue;
	if (V2_read_line (Map, Points, index) < 0)
	    return -1;
	count++;
	set_cat(cat);
	plot_line (Points->x, Points->y, Points->n_points);
    }
    return count;
}

static
plot_line (x, y, n)
    double *x, *y;
{
    while (--n > 0)
    {
	G_plot_line2 (x[0],y[0],x[1],y[1]);
	x++;
	y++;
    }
}
