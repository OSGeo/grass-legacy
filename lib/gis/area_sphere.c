#include <math.h>
#include "gis.h"
#include "pi.h"

static double M;

/*
 * r is radius of sphere, s is a scaling factor
 */

/*!
 * \brief initialize calculations for sphere
 *
 * Initializes raster area calculations for a sphere.
 * The radius of the sphere is <b>r</b> and <b>s</b> is a scale factor to
 * allow for calculations of a part of the zone (see
 * <i>G_begin_zone_area_on_ellipsoid</i>).
 *
 *  \param r
 *  \param s
 *  \return int
 */

int G_begin_zone_area_on_sphere (double r,double s)
{
    return (M = s * 2.0 * r * r * PI);
}

/*
 * part of integral for area between two latitudes
 */
double G_darea0_on_sphere ( register double lat)
{
    return ( M * sin (Radians(lat)) );
}

/*
 * this routine shows how to calculate area between two lats, but
 * isn't efficient for row by row since G_darea0() will be called
 * twice for the same lat, once as a south then again as a north
 */

/*!
 * \brief area between latitudes
 *
 *  Returns the area between latitudes <b>north</b> and
 * <b>south</b> scaled by the factor <b>s</b> passed to
 * <i>G_begin_zone_area_on_sphere.</i>
 *
 *  \param north
 *  \param south
 *  \return double
 */

double G_area_for_zone_on_sphere (
    register double north,
    register double south)
{
    return (G_darea0_on_sphere(north) - G_darea0_on_sphere(south));
}
