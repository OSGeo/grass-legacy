#include "pi.h"

static double E;
static double M;

extern double sin();
extern double log();
extern double sqrt();

/*
 * a is semi-major axis, e2 is eccentricity squared, s is a scale factor
 * code will fail if e2==0 (sphere)
 */
G_begin_zone_area_on_ellipsoid (a, e2, s)
    double a, e2, s;
{
    E = sqrt(e2);
    M = s * a * a * PI * (1 - e2) / E;
}

/*
 * this routine is part of the integral for the area between two latitudes
 */
double
G_darea0_on_ellipsoid (lat)
    register double lat;
{
    register double x;

    x = E * sin (Radians(lat));

    return (M* (x/(1.0-x*x) + 0.5*log((1.0+x)/(1.0-x))));
}

/*
 * this routine shows how to calculate area between two lats, but
 * isn't efficient for row by row since G_darea0() will be called
 * twice for the same lat, once as a south then again as a north
 */
double
G_area_for_zone_on_ellipsoid (north, south)
    register double north, south;
{
    return (G_darea0_on_ellipsoid (north) - G_darea0_on_ellipsoid (south));
}
