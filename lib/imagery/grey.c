#include <stdlib.h>
#include <grass/gis.h>
#include <grass/imagery.h>
/***************************************************************
 * I_grey_scale (histo, colr)
 *
 *  struct Histogram *histo;
 *  struct Colors *colr;
 *
 *  given the histogram (histo),  the colr table
 *  is filled with a contrast stretched grey scale
 ***************************************************************/
int I_grey_scale (
    struct Histogram *histo,
    struct Colors *colr)
{
    unsigned char *map, *xm;
    CELL min,max;
    CELL i;
    int grey;

    I_histo_eq (histo, &map, &min, &max);
    G_init_colors (colr);
    xm = map;
    for (i = min; i <= max; i++)
    {
	grey = *xm++;
	G_set_color (i, grey, grey, grey, colr);
    }
    free (map);

    return 0;
}
