/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       coorcnv library
 * AUTHOR(S):    Original author unknown - probably CERL
 *               Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE:      converting lat lon to geocentric coordinates 
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/***********************************************************************
 * CC_ll2geo (a, e2, lat, lon, h, x, y, z)
 *
 * converts lat, lon to geocentric coordinates
 *
 * inputs
 *   a    major axis of local spheroid
 *   e2   eccentricity squared of spheroid
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
 *********************************************************************/

#include <math.h>
#include "CC.h"
#include "cc.h"

int 
CC_ll2geo (double a, double e2, double lat, double lon, double h, double *x, double *y, double *z)
{
    double phi;		/* latitude in radians  */
    double lambda;	/* longitude in radians */
    double sinphi;	/* sin(phi)             */
    double cosphi;	/* cos(phi)             */
    double N;           /* Querkruemmungsradius = 
			 * radius of curvature in the prime vertical */

    phi    = lat * SECONDS_TO_RADIANS;
    lambda = lon * SECONDS_TO_RADIANS;
    sinphi = sin(phi);
    cosphi = cos(phi);
    N      = a / sqrt (1 - e2 * sinphi * sinphi) ;

    *x = (N + h) * cosphi * cos (lambda);
    *y = (N + h) * cosphi * sin (lambda);
    *z = (N + h - N * e2) * sinphi;

    return 0;
}

int
CC_lld2geo(double a, double e2, double lat, double lon, double h, double *x, double *y, double *z)
{
  lat = lat * 3600;
  lon = lon * 3600;
  if (CC_ll2geo(a, e2, lat, lon, h, x, y, z) == 1)
    return 0;
  
  return 1;
}
