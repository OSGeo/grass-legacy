#include "gis.h"
#include "local_proto.h"

int check_stats (char *name, int commandmode)
{
    struct Histogram histogram;
    struct Categories cats;
    struct Range range;
    struct FPRange fprange;
    char *mapset;
    int i;
    int cats_ok;
    int max;
    char question[100];
    RASTER_MAP_TYPE data_type;

    mapset = G_mapset();
    data_type = G_raster_map_type(name, mapset);

    /* note return value 1 = do hitreturn(), 0 otherwise */

    if (! commandmode)
    {
     sprintf (question, "Update the stats (histogram,range) for [%s]? ", name);
     if (!G_yes (question, 0)) return 0;
    }
    /* always run calculation if in commandmode */

    fprintf (stdout,"\n  Updating the stats for [%s]\n\n", name);
    if(!do_histogram (name, mapset))
	return 1;
    if (G_read_histogram (name, mapset, &histogram) <= 0)
	return 1;

    if(data_type == CELL_TYPE)
        G_init_range (&range);
    else
	G_init_fp_range (&fprange);

    i = G_get_histogram_num (&histogram);
    while (i >= 0){
	if(data_type == CELL_TYPE)
	    G_update_range (G_get_histogram_cat(i--,&histogram), &range);
	else
	    G_update_fp_range (G_get_histogram_cat(i--,&histogram), &fprange);
    }
    if(data_type == CELL_TYPE)
        G_write_range (name, &range);
    else
        G_write_fp_range (name, &fprange);

    cats_ok = G_read_cats (name, mapset, &cats) >= 0;
    max = (data_type == CELL_TYPE ? range.max : fprange.max);
    if (!cats_ok)
	G_init_cats (max, "", &cats);
    else if (cats.num != max)
    {
	cats.num = max;
	cats_ok = 0;
    }
    if (!cats_ok)
    {
	fprintf (stdout,"   Updating the number of categories for [%s]\n\n", name);
	G_write_cats (name, &cats);
    }
    G_free_histogram (&histogram);
    G_free_cats (&cats);
    return 1;
}
