#include "distance.h"
make_support_files (output, argc, argv, units)
    char *output;
    char *argv[];
    char *units;
{
    struct Categories pcats;
    struct Colors pcolr;

    CELL cat;
    char label[128];

    G_init_cats ((CELL) 1, "Distance Zones", &pcats);

    G_set_cat (cat=1, "distances calculated from these locations", &pcats);
    for (cat = 0; cat < argc; cat++)
    {
	sprintf (label, "%s %s", argv[cat], units);
	G_set_cat (cat+ZONE_INCR, label, &pcats);
    }
    
    G_write_cats (output, &pcats);
    G_free_cats(&pcats);

    /*
    G_make_random_colors (&pcolr, (CELL) 1, (CELL)argc+ZONE_INCR-1);
    G_write_colors (output, G_mapset(), &pcolr);
    G_free_colors(&pcolr);
    */
}
