#include "Vect.h"
#include "gis.h"
#include "null.h"

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
	if (!line_ok (Map, index))
	    continue;
	get_line_label (Map, index, &cat);
	if (!is_labeled(cat)) /* Don't rasterize unlabeled lines */
	    cat = 1;
/*
	    continue;
*/
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

static
line_ok (Map, index)
    struct Map_info *Map;
    int index;
{
    if (!LINE_ALIVE(&Map->Line[index]))
	return 0;
    if (Map->Line[index].type == DOT)
	return 0;
    return 1;
}
