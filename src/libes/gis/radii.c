#include "gis.h"
#include "pi.h"

extern double sin(), sqrt();

/****************************************************************
 Various formulas for the ellipsoid.
 Reference: Map Projections by Peter Richardus and Ron K. Alder
	    University of Illinois Library Call Number: 526.8 R39m
 Parameters are:
    lon = longitude of the meridian
    a   = ellipsoid semi-major axis
    e2  = ellipsoid eccentricity squared


  meridional radius of curvature (p. 16)
                        2
               a ( 1 - e )
       M = ------------------
                 2   2    3/2
           (1 - e sin lon)

  transverse radius of curvature (p. 16)

                    a
       N = ------------------
                 2   2    1/2
           (1 - e sin lon)

  radius of the tangent sphere onto which angles are mapped
  conformally (p. 24)

       R = sqrt ( N * M )

***************************************************************************/

/*!
 * \brief meridional radius of curvature
 *
 * Returns the meridional radius of
 * curvature at a given longitude:
  \f$
   \rho = \frac{a (1-e^2)}{(1-e^2\sin^2 lon)^{3/2}}
  \f$
 * 
 *
 *  \param lon
 *  \param a
 *  \param e2
 *  \return double
 */

double
G_meridional_radius_of_curvature (lon, a, e2)
    double lon, a, e2;
{
    double x;
    double s;

    s = sin (Radians(lon));
    x = 1 - e2 * s*s;

    return a * (1 - e2) / (x * sqrt(x));
}



/*!
 * \brief transverse radius of curvature
 *
 * Returns the transverse radius of
 * curvature at a given longitude:
  \f$
   \nu = \frac{a}{(1-e^2\sin^2 lon)^{1/2}}
  \f$
 * 
 *
 *  \param lon
 *  \param a
 *  \param e2
 *  \return double
 */

double
G_transverse_radius_of_curvature (lon, a, e2)
    double lon, a, e2;
{
    double x;
    double s;

    s = sin (Radians(lon));
    x = 1 - e2 * s*s;

    return a / sqrt(x);
}


/*!
 * \brief radius of conformal tangent sphere
 *
 * Returns the radius of the
 * conformal sphere tangent to ellipsoid at a given longitude:
  \f$
    r = \frac{a (1-e^2)^{1/2}}{(1-e^2\sin^2 lon)}
  \f$
 * 
 *
 *  \param lon
 *  \param a
 *  \param e2
 *  \return double
 */

double
G_radius_of_conformal_tangent_sphere (lon, a, e2)
    double lon, a, e2;
{
    double x;
    double s;

    s = sin (Radians(lon));
    x = 1 - e2 * s*s;

    return a * sqrt (1 - e2) / x;
}
