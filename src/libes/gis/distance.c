#include "gis.h"

/*
 * This factor is to handle state plane systems which have
 * coordinate grids in feet
 */

#define FEET_TO_METERS .3048

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
	break;
    case PROJECTION_SP:
	factor = FEET_TO_METERS;
	break;
    default:
	break;	/* assume meter grid */
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
