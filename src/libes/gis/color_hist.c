/**********************************************************************
 *
 *  G_make_histogram_eq_colors (colors, statf)
 *
 *   struct Colors *colors      struct to hold colors
 *   struct Cell_stats *statf   cell stats info
 *
 *  Generates histogram equalized grey scale from
 *  cell stats structure info.
 *  Color range is 0-255.
 *
 **********************************************************************/

#include "gis.h"
G_make_histogram_eq_colors (colors, statf)
    struct Colors *colors;
    struct Cell_stats *statf;
{
    long count, total;
    CELL prev,cat;
    double span, sum;
    int first;
    int x, grey;

    G_init_colors (colors);
    G_add_color_rule ((CELL) 0, 0, 0, 0, (CELL) 0, 0, 0, 0, colors);

    total = 0;

    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
	if (cat && count > 0)
	    total += count;
    if (total <= 0)
	return ;

    span = total/256.0;
    first = 1;
    grey = 0;
    sum = 0.0;

    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
    {
	if (cat == 0 || count <= 0)
	    continue;
	x = (sum + (count/2.0))/span;
	if (x < 0) x = 0;
	else if (x > 255) x = 255;
	sum += count;
	if (first)
	{
	    prev = cat;
	    grey = x;
	    first = 0;
	}
	else if (grey != x)
	{
	    G_add_color_rule (prev, grey, grey, grey, cat-1, grey, grey, grey, colors);
	    grey = x;
	    prev = cat;
	}
    }
    if (!first)
    {
	G_add_color_rule (prev, grey, grey, grey, cat, grey, grey, grey, colors);
    }
}
