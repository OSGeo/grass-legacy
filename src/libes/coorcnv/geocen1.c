#include <math.h>
#include "CC.h"
/*******************************************************************
 * CC_ll2geo (a, e, lat, lon, h, x, y, z)
 *
 * converts lat, lon to geocentric coordinates
 *
 * inputs
 *   a    major axis of local spheroid
 *   e    eccentricity squared of spheroid
 *   lat  latitude in seconds of arc
 *   lon  longitude in seconds of arc
 *   h    height above spheroid at this lat, lon
 *
 * outputs
 *   x,y,z  geocentric coordinates
 *
 * note:
 *   Usually h can not be known. It can be approxiamted by
 *   0 or by the average elevation above sea level or whatever.
 *   At this time the effect of h in the equations is not
 *   known
 *
 * uses math library: load with -lm 
 ***************************************************************/

#define SECONDS_TO_RADIANS 4.84813681109536e-6

int 
CC_ll2geo (double a, double e, double lat, double lon, double h, double *x, double *y, double *z)
{
    double phi;		/* latitude in radians */
    double lambda;	/* longitude in radians */
    double sinphi;	/* sin(phi)             */
    double cosphi;	/* cos(phi)             */
    double N;

    phi    = lat * SECONDS_TO_RADIANS;
    lambda = lon * SECONDS_TO_RADIANS;
    sinphi = sin(phi);
    cosphi = cos(phi);
    N = a / sqrt (1 - e * sinphi * sinphi) ;

    *x = (N + h) * cosphi * cos (lambda);
    *y = (N + h) * cosphi * sin (lambda);
    *z = (N + h - N*e) * sinphi;

    return 0;
}
