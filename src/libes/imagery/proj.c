#include "gis.h"
I_must_be_imagery_projection()
{
    int proj;
    char *G_projection_name();
    if ((proj = G_projection()) == PROJECTION_XY)
	return;

    printf ("ERROR: LOCATION %s has a projection of %d (%s)\n",
	G_location(), proj, G_database_projection_name());
    printf ("\nImagery data must be extracted into a location\n");
    printf ("which has a projection of %d (row-column only)\n", PROJECTION_XY);
    printf ("\nUnable to proceed\n");
    exit(1);
}
