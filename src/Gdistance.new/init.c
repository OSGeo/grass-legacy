#include "distance.h"
init_grass(pgm)
    char *pgm;
{
    double a,e2;

    G_gisinit (pgm);
    G_get_set_window(&window);
    if (window.proj == PROJECTION_LL)
    {
	G_get_ellipsoid_parameters (&a, &e2);
	G_begin_geodesic_distance  (a, e2);
	wrap_ncols = (360.0 - (window.east-window.west))/window.ew_res + 1.1;
	    /* add 1.1 instead of 1 to insure that we round up, not down */
    }
    else
    {
	wrap_ncols = 0;
	if (window.proj == PROJECTION_SP)
	    meters_to_grid = 1.0/FEET_TO_METERS;
	else
	    meters_to_grid = 1.0;
    }
}
