#include "gis.h"

int 
get_histo (char *name, char *mapset, struct Histogram *histo)
{
    struct Range range;
    int i;
    CELL cat;

/* read the histogram file */
    G_suppress_warnings (1);
    i = G_read_histogram (name, mapset, histo);
    G_suppress_warnings (0);
    if (i > 0)
    {
    /* force cat 0 to appear */
	G_set_histogram ((CELL)0, (long)1, histo);
	G_sort_histogram (histo);
	return 1;
    }

/* fake the histogram from the range */
    G_init_histogram (histo);
    G_set_histogram ((CELL)0, (long)1, histo);
    if (G_read_range (name, mapset, &range) < 0)
	return 0;
    for (cat = range.min; cat <= range.max; cat++)
	G_set_histogram (cat, (long)1, histo);
    G_sort_histogram (histo);
    return 1;
}
