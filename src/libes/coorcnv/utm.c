/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:   	 coorcnv library    
 * AUTHOR(S):    Original author unknown - probably CERL
 *               Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE:      utm to lat lon and lat lon to utm conversion
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/***********************************************************************
 * utm to lat/lon conversion (u2ll)
 * lat/lon to utm conversion (ll2u)
 *
 * CC_u2ll_spheroid (spheroid_name)
 *     char *spheroid_name
 *
 *     must be called first. sets the spheroid parameters
 *     for the ellipse 'spheroid_name' (see get_spheroid.c for known spheroids)
 *     (used for conversions in either direction)
 *     returns -1 unrecognized spheroid, -2 internal error, 1 otherwise
 *
 * CC_u2ll_spheroid_parameters (a,e2)
 *    double a,e2;
 *
 *    called by CC_u2ll_spheroid() to set the ellipsoid major axis 'a'
 *    and eccentricity squared 'e'. can be called directly for
 *    unknown ellipsoids.
 *    returns -2 illegal values for a or e, 1 otherwise
 *
 * CC_u2ll_zone (zone)
 *   must be called before CC_u2ll_north()
 *   set utm 'zone' must be non-zero.
 *   positive means northern hemisphere
 *   negative means southern hemisphere
 *   used to set the longitude of the central merdian
 *   (only used for utm to lat/lon conversions)
 *
 * CC_u2ll_north (north)
 *   double north
 *
 *   set the utm north. must be called before CC_u2ll.
 *   returns -1 if related rectifying latitude exceed 1.47 radians
 *            1 otherwise
 *
 * CC_u2ll (east, lat, lon)
 *  double east
 *  double *lat, *lon
 *
 *  compute lat,lon from east
 *  CC_u2ll_north() already called with north.
 *  returns -1 if lon is more than .16 radians from center of utm zone
 *           1 otherwise
 ***********************************************************************/

#include <math.h>
#include "CC.h"

/* #define abs(x) ((x)<0?-(x):(x)) */

#define RADIANS_TO_SECONDS 206264.8062470964 
#define SECONDS_TO_RADIANS 4.84813681109536e-6

static double dabs(double);

static double a1,a2,a3,a4;	/* lat coef: geodetic to rectifying */
static double a5 = 5.0e5;	/* false easting (UTM) */
static double a6;		/* false northing */
static double a7 = 0;		/* M0? */
static double a8 = .9996;	/* UTM scale factor at central meridian */
static double a9;		/* central meridian in seconds */
static double a10;		/* radius of curvature */
static double a11,a12,a13,a14;	/* lat coef: rectifying to geodetic */
static double a15;		/* major axis */
static double a16;		/* eccentricity squared */

static double b1,b2,b3,b4;	/* intermediate values */
static double b5,b6,b7,b8;
static double b9,b10,b11,b12;

int 
CC_u2ll_spheroid (char *spheroid_name)
{
    double a, e2;

    if (CC_get_spheroid (spheroid_name, &a, &e2))
	return CC_u2ll_spheroid_parameters (a, e2);
    return -1;      /* unknown spheroid */
}

int 
CC_u2ll_spheroid_parameters (double a, double e2)
{
    double x, x2, x3, x4;

    if (a < 0.0 || e2 < 0.0 || e2 > 1.0)
	return -2;  /* illegal values */

    a15 = a;
    a16 = e2;

    x = (((e2 * (7.0/32.0) + 5.0/16.0) * e2 + .5) * e2 + 1.0) * e2 * .25;
    x2 = x * x;
    x3 = x * x2;
    x4 = x * x3;

    /* coefficients to convert geodetic to rectifying latitude */
    a1 = -(((x * (195.0/64.0) + 3.25) * x + 3.75) * x + 3.0) * x ;
    a2 = (((1455.0/32.0) * x + 70.0/3.0) * x + 7.5) * x2;
    a3 = -((70.0/3.0) + x * (945.0/8.0)) * x3;
    a4 = (315.0/4.0) * x4;

    /* coefficients to convert rectifying to geodetic latitude */
    a11 = (((7.75 - (657.0/64.0) * x) * x - 5.25) * x + 3.0) * x;
    a12 = (((5045.0/32.0) * x - (151.0/3.0)) * x + 10.5) * x2;
    a13 = ((151.0/3.0) - (3291.0/8.0) * x) * x3;
    a14 = (109.0/4.0) * x4;

    /* radius of curvature */
    a10 = (((225.0/64.0) * x2 + 2.25) * x2 + 1.0) * (1.0 - x2) * (1.0 - x) * a;

    return 1;
}

int 
CC_u2ll_zone (int zone)
{
    double utz;

    /* set false northing (a6), compute central meridian (a9) */
    if (zone < 0)
    {
	a6 = 10.0e6;
	utz = 30.0 + zone;
    }
    else
    {
	a6 = 0.0;
	utz = 30.0 - zone;
    }

    a9 = (utz * 6.0 + 3.0) * 3600.0;

    return 0;
}

int 
CC_u2ll_north (double north)
{
    double sinw, cosw;
    double t, ts, rn, rn2, rn4, rn6, rn8;
    double etas;

    b10 = ((north - a6) / a8 + a7) / a10;
    if (dabs(b10) > 1.47)
	return -1; /* rectifying lat exceeds 1.47 radians, ~84.15.30 */
    
    sinw = sin(b10);
    cosw = cos(b10);
    b12  = cosw * cosw;
    b11  = (((a14 * b12 + a13) * b12 + a12) * b12 + a11) * sinw * cosw + b10;

    sinw = sin(b11);
    cosw = cos(b11);
    rn   = sqrt (1.0 - a16*sinw*sinw) * 1.0e6 / a15;
    rn2 = rn * rn ;
    rn4 = rn2 * rn2 ;
    rn6 = rn2 * rn4 ;
    rn8 = rn4 * rn4 ;
    t    = sinw/cosw;
    ts   = t * t;

    b12  = cosw * cosw;
    etas = a16 * b12 / (1.0 - a16);

    b1   = rn/cosw;
    b2   = -t * (1.0 + etas) * rn2 / 2.0 ;
    b3   = - (1.0 + ts + ts + etas) * b1 * rn2 / 6.0 ;
    b4   = ((( -6.0 - etas * 9.0) * etas + 3.0) * ts + (6.0 - etas * 3.0) * etas + 5.0 ) * t * rn4 / 24.0 ;
    b5   = ((ts * 24.0 + etas * 8.0 + 28.0) * ts + etas * 6.0 + 5.0) * b1 * rn4 / 120.0 ;
    b6   = (((etas * 45.0 - 45.0) * ts + etas * 162.0 -90.0) * ts - etas * 107.0 - 61.0) * t * rn6 / 720.0 ;
    b7   = -(((ts * 720.0 + 1320.0) * ts + 662.0) * ts + 61.0) * b1 * rn6 / 5040.0 ;
    b8   = (((ts * 1575.0 + 4095.0) * ts + 3633.0) * ts + 1385.0) * t * rn8 / 40320.0;

    return 1;
}

int 
CC_u2ll (double east, double *lat, double *lon)
{
    double b9, b10;

    b9 = ((a5 - east) * 1.0e-6) / a8 ;
    if (dabs(b9) > a15 * 2.0e-7)
	return -1;   /* utm easting to far from center of zone */

    b10 = b9 * b9 ;
    *lat = ((((b8 * b10 + b6) * b10 + b4) * b10 + b2) * b10 + b11) * RADIANS_TO_SECONDS ;
    *lon = (((b7 * b10 + b5) * b10 + b3) * b10 + b1) * b9 * RADIANS_TO_SECONDS + a9 ;

    return 1;
}

int 
CC_ll2u (double lat, double lon, double *east, double *north, int *zone)
{
    int deg;
    double sinp, cosp;
    double etas, t, ts, rn;
    double c1,c2,c3;

    /* if zone is != 0, force into this zone, otherwise compute the zone */
    if (*zone == 0)
    {
	if (lon < 0)	/* eastern zones */
	{
	    deg = -lon / 3600;
	    *zone = 31 + deg / 6;
	}
	else		/* western zones */
	{
	    deg = lon / 3600 ;
	    *zone = 30 - deg / 6;
	}
	if (lat < 0)
	    *zone = -(*zone) ;
    }

    /* now, set a6,a9 */
    (void) CC_u2ll_zone (*zone);	

    if (dabs(lat) > 302400.0)
	return -1;   /* latitude above 84 degrees */

    b10 = (a9 - lon) * SECONDS_TO_RADIANS;
    if (dabs(b10) > .16)
	return -2;   /* longitude to far from center of utm zone */

    b9 = lat * SECONDS_TO_RADIANS;
    sinp = sin (b9);
    cosp = cos (b9);
    rn = a15 / sqrt (1.0 - a16 * sinp * sinp);
    t = sinp / cosp;
    ts = t * t;
    c1 = cosp * cosp;
    c2 = c1 * c1;
    c3 = c1 * c2;
    etas = a16 * c1 / (1.0 - a16);

    b1 = rn * cosp;
    b3 = (1.0 - ts + etas) * b1 * c1 / 6.0;
    b5 = ((ts - 18.0) * ts + 5.0 + (14.0 - 58.0 * ts) * etas) * b1 * c2 / 120.0;
    b7 = (((179.0 -ts) * ts - 479.0) * ts + 61.0) * b1 * c3 / 5040.0 ;
    b12 = b10 * b10;

    *east = (((b7 * b12 + b5) * b12 + b3) * b12 + b1) * b10 * a8 + a5;

    b2 = rn * c1 * t / 2.0;
    b4 = (etas * (9.0 + 4.0 * etas) + 5.0 - ts) * b2 * c1 / 12.0;
    b6 = ((ts - 58.0) * ts + 61.0 + (270.0 - 333.0 * ts) * etas) * b2 * c2 / 360.0;
    b8 = (((543.0 - ts) * ts - 3111.0) * ts + 1385.0) * b2 * c3 / 20160.0;

    *north = (((b8 * b12 + b6) * b12 + b4) * b12 + b2) * b12 +
	     ((((a4 * c1 + a3) * c1 + a2) * c1 + a1) * sinp * cosp + b9)
	     * a10;

    *north = (*north - a7) * a8 + a6;

    return 1;
}

static double 
dabs(double x)
{
    return x < 0 ? -x : x;
}
