#include "gis.h"

/* WARNING: this code is preliminary and may be changed,
 * including calling sequences to any of the functions
 * defined here
 */

static int projection = 0;
static double factor = 1.0;

G_begin_distance_calculations()
{
    double a, e2;

    factor = 1.0;
    switch (projection = G_projection())
    {
    case PROJECTION_LL:
	G_get_ellipsoid_parameters (&a, &e2);
	G_begin_geodesic_distance (a, e2);
	return 2;
    default:
	factor = G_database_units_to_meters_factor();
	if (factor <= 0.0)
	{
	    factor = 1.0;          /* assume meter grid */
	    return 0;
	}
	return 1;
    }
}

double
G_distance (e1,n1,e2,n2)
    double e1,n1,e2,n2;
{
    double G_geodesic_distance();
    double hypot();

    if (projection == PROJECTION_LL)
	return G_geodesic_distance (e1, n1, e2, n2);
    else
	return factor * hypot (e1-e2,n1-n2);
}
