#include "gis.h"
I_must_be_imagery_projection()
{
    int proj;
    char *G_projection_name();
    if ((proj = G_projection()) == 0)
	return;

    printf ("ERROR: LOCATION %s has a projection of %d (%s)\n",
	G_location(), proj, G_projection_name(proj));
    printf ("\nImagery data must be extracted into a location\n");
    printf ("which has a projection of 0 (row-column only)\n");
    printf ("\nUnable to proceed\n");
    exit(1);
}
