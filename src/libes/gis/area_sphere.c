#include "pi.h"

static double M;

extern double sin();

/*
 * r is radius of sphere, s is a scaling factor
 */
G_begin_zone_area_on_sphere (r, s)
    double r, s;
{
    M = s * 2.0 * r * r * PI;
}

/*
 * part of integral for area between two latitudes
 */
double
G_darea0_on_sphere (lat)
    register double lat;
{
    return ( M * sin (Radians(lat)) );
}

/*
 * this routine shows how to calculate area between two lats, but
 * isn't efficient for row by row since G_darea0() will be called
 * twice for the same lat, once as a south then again as a north
 */
double
G_area_for_zone_on_sphere (north, south)
    register double north, south;
{
    return (G_darea0_on_sphere(north) - G_darea0_on_sphere(south));
}
