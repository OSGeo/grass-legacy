/**************************************************************
* I_histo_eq (histo, map, min, max)
*
*   struct Histogram *histo;    histogram as returned by I_get_histo()
*   unsigned char **map;        equalized category mapping
*   CELL *min, *max;            min,max category for map
*
* perform histogram equalization
* inputs: histo
* outputs map,min,max
****************************************************************/
#include "gis.h"
I_histo_eq (histo, map, min, max)
    struct Histogram *histo;
    unsigned char **map;
    CELL *min, *max;
{
    int i;
    int x;
    CELL cat, prev;
    double total;
    double sum;
    double span;
    int ncats;
    long count;
    unsigned char *xmap;
    int len;
    int first, last;

    ncats = G_get_histogram_num (histo);
    if (ncats == 1)
    {
	*min = *max = G_get_histogram_cat (0, histo);
	*map = xmap = (unsigned char *) G_malloc (1);
	*xmap = 0;
	return;
    }
    if((*min = G_get_histogram_cat (first = 0, histo)) == 0)
	*min = G_get_histogram_cat (++first, histo);
    if((*max = G_get_histogram_cat (last = ncats-1, histo)) == 0)
	*max = G_get_histogram_cat (--last, histo);
    len = *max - *min + 1;
    *map = xmap = (unsigned char *) G_malloc (len);

    total = 0;
    for (i = first; i <= last; i++)
    {
	if (G_get_histogram_cat (i, histo) == 0)
		continue;
	count = G_get_histogram_count (i, histo);
	if (count > 0) total += count;
    }
    if (total <= 0)
    {
	for (i = 0; i < len; i++)
	    xmap[i] = 0;
	return;
    }

    span = total / 256 ;

    sum = 0.0;
    cat = *min-1;
    for (i = first; i <= last; i++)
    {
	prev = cat+1;
	cat   = G_get_histogram_cat (i, histo);
	count = G_get_histogram_count (i, histo);
	if (count < 0 || cat == 0)
	    count = 0;
	x = (sum + (count / 2.0)) / span;
	if (x < 0) x = 0;
	else if (x > 255) x = 255;
	sum += count;

	while (prev++ <= cat)
	    *xmap++ = x ;
    }
}

#ifdef COMMENT_OUT
I_histo_eq_red (histo, map)
    int *histo;
    int *map;
{
    int r,g,b;
    I_get_color_levels (&r,&g,&b);
    I_histo_eq (histo, map, r);
}

I_histo_eq_grn (histo, map)
    int *histo;
    int *map;
{
    int r,g,b;
    I_get_color_levels (&r,&g,&b);
    I_histo_eq (histo, map, g);
}

I_histo_eq_blu (histo, map)
    int *histo;
    int *map;
{
    int r,g,b;
    I_get_color_levels (&r,&g,&b);
    I_histo_eq (histo, map, b);
}
#endif
