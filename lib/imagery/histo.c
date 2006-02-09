#include <grass/imagery.h>
#include <grass/gis.h>

int I_get_histogram( char *name,char *mapset, struct Histogram *histogram)
{
    CELL i,min,max;
    struct Range range;

    if (G_read_histogram (name, mapset, histogram) > 0)
	return 1;

/* fake the histogram */
    G_read_range (name, mapset, &range);
    G_get_range_min_max (&range, &min, &max);
    G_init_histogram (histogram);
    for (i = min; i <= max; i++)
	G_set_histogram (i, (long)1, histogram);
    G_sort_histogram (histogram);
    return 1;
}
