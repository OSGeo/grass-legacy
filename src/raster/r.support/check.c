#include "gis.h"

check_stats (name)
    char *name;
{
    struct Histogram histogram;
    struct Categories cats;
    struct Range range;
    char *mapset;
    int i;
    int cats_ok;
    char question[100];

    mapset = G_mapset();
    /* note return value 1 = do hitreturn(), 0 otherwise */

    sprintf (question, "Update the stats (histogram,range) for [%s]? ", name);
    if (!G_yes (question, 0)) return 0;

    printf ("\n  Updating the stats for [%s]\n\n", name);
    if(!do_histogram (name, mapset))
	return 1;
    if (G_read_histogram (name, mapset, &histogram) <= 0)
	return 1;

    G_init_range (&range);
    i = G_get_histogram_num (&histogram);
    while (i >= 0)
	G_update_range (G_get_histogram_cat(i--,&histogram), &range);
    G_write_range (name, &range);

    cats_ok = G_read_cats (name, mapset, &cats) >= 0;
    if (!cats_ok)
	G_init_cats (range.pmax, "", &cats);
    else if (cats.num != range.pmax)
    {
	cats.num = range.pmax;
	cats_ok = 0;
    }
    if (!cats_ok)
    {
	printf ("   Updating the number of categories for [%s]\n\n", name);
	G_write_cats (name, &cats);
    }
    G_free_histogram (&histogram);
    G_free_cats (&cats);
    return 1;
}
