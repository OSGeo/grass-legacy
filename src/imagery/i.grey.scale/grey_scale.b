#include "gis.h"
grey_scale(name,mapset)
    char *name;
    char *mapset;
{
    struct Colors colr;
    struct Histogram histogram ;

    if(I_get_histogram (name, mapset, &histogram) < 0)
	return;
    /*
    G_make_histo_grey_scale (&colr, &histogram);
    */
    I_grey_scale (&histogram, &colr);

    G_write_colors (name, mapset, &colr);
    G_free_histogram (&histogram);
    G_free_colors (&colr);
    printf ("[%s in %s] now has a grey scale color table\n", name, mapset);
}
