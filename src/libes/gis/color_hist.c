#include "gis.h"
/***************************************************************
 * G_make_histo_grey_scale (colr, histo)
 *
 *  struct Histogram *histo;
 *  struct Colors *colr;
 *
 *  given the histogram (histo),  the colr table
 *  is filled with a contrast stretched grey scale
 ***************************************************************/
G_make_histo_grey_scale (colr, histo)
    struct Colors *colr;
    struct Histogram *histo;
{
    unsigned char *map, *xm;
    CELL min,max;
    CELL i;
    int grey;

    G_histogram_eq (histo, &map, &min, &max);
    G_init_colors (colr);
    xm = map;
    for (i = min; i <= max; i++)
    {
	grey = *xm++;
	G_set_color (i, grey, grey, grey, colr);
    }
    free (map);
}
