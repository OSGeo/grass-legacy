#include "distance.h"

int make_support_files (char *output, char *units)
{
    struct Categories pcats;

    CELL cat;
    char label[128];

    G_init_cats ((CELL) 1, "Distance Zones", &pcats);

    G_set_cat (cat=1, "distances calculated from these locations", &pcats);
    for (cat = 0; cat < ndist; cat++)
    {
	sprintf (label, "%s %s", distances[cat].label, units);
	G_set_cat (cat+ZONE_INCR, label, &pcats);
    }
    
    G_write_cats (output, &pcats);
    G_free_cats(&pcats);

    return 0;
}
