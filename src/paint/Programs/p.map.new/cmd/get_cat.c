#include "gis.h"
#include "misc.h"
#include "colormode.h"
#include "parms.h"
#include "local_proto.h"


#define SKIP0
#undef PERCENT_COVER

int get_cat (struct Cell_stats *statf)
{
    char *name;

    CELL *index;
    CELL catnum;
    DCELL dmin, dmax;
    long count;
    int cats_appearing;
	int total;
	int len;
	int maxname;

fprintf (stdout," get_cat\n");
    cats_appearing = 0;
    total = 0;
    index = NULL;
    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&catnum, &count, statf))
    {
#ifdef SKIP0
	if (catnum == 0)
	    continue;
#endif
	cats_appearing++;
	index = (CELL *) G_realloc (index, cats_appearing * sizeof (CELL));
	index[cats_appearing-1] = catnum;
	total += count;
    }
    if (cats_appearing <= 0)
	return;

    if (total <= 0) total = 1.0;

	maxname = 0;

    G_rewind_cell_stats (statf);
    while (G_next_cell_stat (&catnum, &count, statf))
    {

#ifdef SKIP0
	if (catnum == 0)
	    continue;
#endif
        if(parms.map_type == CELL_TYPE)
	   name = G_get_cat (catnum, &parms.pcats);
        else
	   name = G_get_ith_d_raster_cat(&parms.pcats, catnum, &dmin, &dmax);
	/*
        if ((len = strlen(name)) > maxname)
            maxname = len;
#ifdef PERCENT_COVER
        sprintf(temp2,"(%.1lf)", (count/total)*100);
        t = strlen(temp2)+len+MIN_DOTS;
#else
	t = len;
#endif
*/
    }

    return 0;
}
