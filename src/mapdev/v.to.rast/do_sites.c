#include "Vect.h"
#include "gis.h"
#include "null.h"
#include "local_proto.h"

static int plot_points (double *,double *,int);
static int site_ok ( struct Map_info *, int);

int do_sites (
    struct Map_info *Map,
    struct line_pnts *Points)
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

static int plot_points (double *x,double *y,int n)
{
/* only plot the first point */
    if (n > 0)
	G_plot_point (*x, *y);

  return 0;
}

static int site_ok ( struct Map_info *Map, int index)
{
    if (!LINE_ALIVE(&Map->Line[index]))
	return 0;
    if (Map->Line[index].type != DOT)
	return 0;
    return 1;
}
