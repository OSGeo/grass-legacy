#include "gis.h"

make_support (input, mapset, raster, npoints, percent, cat_zero)
    char *input, *mapset, *raster, *npoints;
    CELL cat_zero;
{
    char title[100];
    struct History hist;
    struct Categories cats;

    if(G_read_cats (input, mapset, &cats) >= 0)
    {
	sprintf (title, "Random sites on [%s in %s]", input, mapset);
	G_set_cats_title (title, &cats);
	if (cat_zero)
	    G_set_cat (cat_zero, "Original data was 0 for these sites", &cats);
	G_write_cats (raster, &cats);
    }


    if (G_read_history (raster, G_mapset(), &hist) >= 0)
    {
	sprintf (hist.datsrc_1, "Based on map [%s in %s]", input, mapset);
	if (percent)
	    sprintf (hist.datsrc_2, "Random sites over %s of the base map",
		npoints);
	else
	    sprintf (hist.datsrc_2, "%s random sites on the base map", npoints);
	G_write_history (raster, &hist);
    }
}

contrast(color) {
    return ((color > 127) ? (color - 127) : (color  + 127));
    }
