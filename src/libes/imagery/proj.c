#include "imagery.h"
#include "gis.h"
int I_must_be_imagery_projection()
{
    int proj;
    char *G_projection_name();
    if ((proj = G_projection()) == PROJECTION_XY)
	return 1;

    fprintf (stdout,"ERROR: LOCATION %s has a projection of %d (%s)\n",
	G_location(), proj, G_database_projection_name());
    fprintf (stdout,"\nImagery data must be extracted into a location\n");
    fprintf (stdout,"which has a xy-projection of %d (row-column only)\n", PROJECTION_XY);
    fprintf (stdout,"\nUnable to proceed\n");
    exit(1);
}
