/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       coorcnv library
 * AUTHOR(S):    Original author unknown - probably CERL
 *               Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE: 	 converting geocentric coordinates to lat lon
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/***********************************************************************
 * CC_geo2ll (a, e2, x, y, z, lat, lon, h, n, stop)
 *
 * convert geocentric coordinates (x,y,z) to geographic (lat,lon,h)
 *
 * inputs
 *  a     major axis of local spheroid
 *  e     square of eccentricity of spheroid
 *  x,y,z geocentric coordinates (in meters)
 *  n     maximum number of iterations when solving equations
 *  stop  stop delta
 *
 * outputs
 *  lat   latitude in arc seconds
 *  lon   longitude in arc seconds
 *  h     height in meters above spheroid
 *
 *  returns: number iterations remaining, or 0 if did not converge
 **********************************************************************/

#include <math.h>
#include "CC.h"
#include "cc.h"

static double dabs(double);

int 
CC_geo2ll (double a, double e2, double x, double y, double z, double *lat, double *lon, double *h, int n, double stop)
{
    double T, t;
    double phi;
    double R;
    double ae;
    double zpt;
    double sinphi;
    int i;

    if (n <= 0) n = 1 ;
    stop = dabs(stop) ;

    R = x*x + y*y ;
    T = z / (1 - e2) - z ;
    ae = a * e2;

    for (i = 1; i <= n; i++)
    {
	zpt = z + T ;
	phi = asin ( zpt / sqrt(R + zpt * zpt) ) ;

	sinphi = sin(phi);
	t = ae * sinphi / sqrt (1 - e2*sinphi*sinphi);

	if (dabs(t-T) <= stop) 
	    break;
	T=t;
    }

    *lat = phi * RADIANS_TO_SECONDS;
    *lon = atan2(y,x) * RADIANS_TO_SECONDS;

    zpt = z + t;
    *h = sqrt (R + zpt * zpt) - a / sqrt (1 - e2 * sinphi * sinphi);

    /* return number of iterations remaining */
    /* ie, if max iterations were used return 0 */
    return i > n ? 0 : i;
}

int
CC_geo2lld(double a, double e2, double x, double y, double z, double *lat, double *lon, double *h)
{
  if (CC_geo2ll(a, e2, x, y, z, lat, lon, h, 100, 1e-11) == 0)
    if (CC_geo2ll(a, e2, x, y, z, lat, lon, h, 500, 1e-11) == 0) {
      printf("error in geo2ll\n");
      return 0;
    }
  
  *lat = *lat / 3600;
  *lon = *lon / 3600;

  return 1;
}


static double 
dabs(double x)
{
    return x < 0 ? -x : x;
}

