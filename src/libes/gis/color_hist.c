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
 **********************************************************************
 *
 *  G_make_histogram_log_colors (colors, statf, min, max)
 *
 *   struct Colors *colors      struct to hold colors
 *   struct Cell_stats *statf   cell stats info
 *
 *  Generates histogram with normalized log transformed grey scale from
 *  cell stats structure info.
 *  Color range is 0-255.
 *
 **********************************************************************/
#include "gis.h"
#include <math.h>

int G_make_histogram_eq_colors (
    struct Colors *colors,
    struct Cell_stats *statf)
{
    long count, total;
    CELL prev=0,cat;
    double span, sum;
    int first;
    int x, grey;

    G_init_colors (colors);
    G_set_null_value_color (0, 0, 0, colors);

    total = 0;

    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
	if (count > 0)
	    total += count;
    if (total <= 0)
	return 0;

    span = total/256.0;
    first = 1;
    grey = 0;
    sum = 0.0;

    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
    {
	if (count <= 0)
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

    return 0;
}


int G_make_histogram_log_colors (
    struct Colors *colors,
    struct Cell_stats *statf,
    int min, int max)
{
    long count, total;
    CELL prev=0,cat;
    int first;
    int x, grey;

    G_init_colors (colors);
    G_set_null_value_color (0, 0, 0, colors);

    total = 0;

    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
	if (count > 0)
	    total += count;
    if (total <= 0)
	return 0;

    first = 1;
    grey = 0;

    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&cat, &count, statf))
    {
	if (count <= 0)
	    continue;
	    
	/* log transform normalized */
	x = (int) ( log(cat)* 255. / log(max) );
	
       	if (x < 0) x = 0;
	else if (x > 255) x = 255;
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

    return 0;
}

