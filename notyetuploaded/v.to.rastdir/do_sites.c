#include "Vect.h"
#include "gis.h"
#include "null.h"

do_sites (Map, Points)
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
	if (!site_ok (Map, index))
	    continue;
	get_site_label (Map, index, &cat);
	if (!is_labeled(cat)) /* Don't rasterize unlabeled sites */
	    continue;
	if (V2_read_line (Map, Points, index) < 0)
	    return -1;
	count++;
	set_cat(cat);
	plot_points (Points->x, Points->y, Points->n_points);
    }
    return count;
}

static
plot_points (x, y, n)
    double *x, *y;
{
/* only plot the first point */
    if (n > 0)
	G_plot_point (*x, *y);
}

static
site_ok (Map, index)
    struct Map_info *Map;
    int index;
{
    if (!LINE_ALIVE(&Map->Line[index]))
	return 0;
    if (Map->Line[index].type != DOT)
	return 0;
    return 1;
}
