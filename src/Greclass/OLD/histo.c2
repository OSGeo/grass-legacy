#include "gis.h"

new_histogram (name, reclass)
    char *name;
    struct Reclass *reclass;
{
    struct Histogram histo;
    CELL cat;
    int i;
    CELL min,max;

    G_suppress_warnings (1);
    i = G_read_histogram (reclass->name, reclass->mapset, &histo);
    G_suppress_warnings (0);
    if (i <= 0) return;

/* reclassify the histogram */

    min = reclass->min;
    max = reclass->max;
    for (i = 0; i < histo.num; i++)
    {
	cat = histo.list[i].cat;
	if (cat < min || cat > max)
	    cat = 0;
	else
	    cat = reclass->table[cat - min];
	histo.list[i].cat = cat;
    }

/*
 * Sort the resultant histogram.
 * This will combine duplicates into single cat
 */
    G_sort_histogram (&histo);
    G_write_histogram (name, &histo);
}
