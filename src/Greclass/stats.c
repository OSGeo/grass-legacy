#include "gis.h"

#define LIST struct Histogram_list

new_stats (name, reclass)
    char *name;
    struct Reclass *reclass;
{
    struct Histogram histo, histo2;
    struct Range range;
    CELL cat, cat2;
    int i, neg, zero, pos;
    CELL min,max;

    min = reclass->min;
    max = reclass->max;

/* read histogram for original file */
    G_suppress_warnings (1);
    i = G_read_histogram (reclass->name, reclass->mapset, &histo);
    G_suppress_warnings (0);
    if (i <= 0) return;

/* compute data rage for reclass */
    G_init_range (&range);

    for (i = 0; i < histo.num; i++)
    {
	cat = histo.list[i].cat;
	if (cat < min || cat > max)
	    continue;
	cat2 = reclass->table[cat - min];
	if (cat2)
	    G_update_range (cat2, &range);
    }
    G_write_range (name, &range);

/* now generate a histogram from the original */

/* allocate histogram list */
    histo2.num = 1;                                   /* for cat 0 */
    if (range.nmin)                                   /* negative cats */
	histo2.num += range.nmax - range.nmin + 1;
    if (range.pmin)                                   /* positive cats */
	histo2.num += range.pmax - range.pmin + 1;

    histo2.list = (LIST *) G_calloc (histo2.num, sizeof (LIST));

/* set all counts to 0 */
    i = 0;
    neg = i - range.nmin;
    if (range.nmin)
	for (cat = range.nmin; cat <= range.nmax; cat++)
	{
	    histo2.list[i].cat = cat;
	    histo2.list[i++].count = 0;
	}

    zero = i;
    histo2.list[i].cat = 0;
    histo2.list[i++].count = 0;

    if (range.pmin)
	for (cat = range.pmin; cat <= range.pmax; cat++)
	{
	    histo2.list[i].cat = cat;
	    histo2.list[i++].count = 0;
	}
    pos = i - range.pmin;

/* go thru original histogram and add into histo2 */
    for (i = 0; i < histo.num; i++)
    {
	cat = histo.list[i].cat;
	if (cat < min || cat > max)
	    cat2 = 0;
	else
	    cat2 = reclass->table[cat - min];
	if (cat2 > 0)
	    histo2.list[pos+cat2].count += histo.list[i].count;
	else if (cat2 == 0)
	    histo2.list[zero].count += histo.list[i].count;
	else
	    histo2.list[neg+cat2].count += histo.list[i].count;
    }
    G_write_histogram (name, &histo2);
}
