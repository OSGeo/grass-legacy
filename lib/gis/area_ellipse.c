#include <math.h>
#include "pi.h"
#include "gis.h"

static double E;
static double M;

/*
 * a is semi-major axis, e2 is eccentricity squared, s is a scale factor
 * code will fail if e2==0 (sphere)
 */

/*!
 * \brief begin area calculations for ellipsoid
 *
 *  Initializes raster area
 * calculations for an ellipsoid, where <b>a</b> is the semi-major axis of the
 * ellipse (in meters), <b>e2</b> is the ellipsoid eccentricity squared, and
 * <b>s</b> is a scale factor to allow for calculations of part of the zone
 * (<b>s</b>=1.0 is full zone, <b>s</b>=0.5 is half the zone, and
 * <b>s</b>=360/ew_res is for a single grid cell).
 * <b>Note.</b> e2 must be positive. A negative value makes no sense, and zero
 * implies a sphere.
 *
 *  \param a
 *  \param e2
 *  \param s
 *  \return int
 */

int G_begin_zone_area_on_ellipsoid (double a,double e2,double s)
{
    E = sqrt(e2);
    M = s * a * a * PI * (1 - e2) / E;

    return 0;
}

/*
 * this routine is part of the integral for the area between two latitudes
 */
double
G_darea0_on_ellipsoid (register double lat)
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

/*!
 * \brief area between latitudes
 *
 *  Returns the area between latitudes
 * <b>north</b> and <b>south</b> scaled by the factor <b>s</b> passed to
 * <i>G_begin_zone_area_on_ellipsoid.</i>
 *
 *  \param north
 *  \param south
 *  \return double
 */

double G_area_for_zone_on_ellipsoid (
    register double north, register double south)
{
    return (G_darea0_on_ellipsoid (north) - G_darea0_on_ellipsoid (south));
}
