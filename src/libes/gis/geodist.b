#include "pi.h"

/* distance from point to point along a geodesic 
 * code from
 *
 * Paul D. Thomas
 * "Spheroidal Geodesics, Reference Systems, and Local Geometry"
 * U.S. Naval Oceanographic Office, p. 162
 * Engineering Library 526.3 T36s
 */

static double boa;
static double f;
static double ff64;
static double al;

extern double sqrt();
extern double sin(), cos(), tan(), atan(), acos();

G_begin_geodesic_distance (a, e2)
    double a, e2;
{
    al = a;
    boa = sqrt (1 - e2);
    f = 1 - boa;
    ff64 = f*f/64;
}

double
G_geodesic_distance (lon1, lat1, lon2, lat2)
    double lon1, lat1, lon2, lat2;
{
    double a, cd, cdtm, ctm, d, dl, dtm, e,
	   kk, kl, l, sd, sdlmr, sdtm, stm,
	   t, t1r, t2r, tm, u, v, x, y;


/*
    while (lon1 > 180.0)
	lon1 -= 360.0;
    while (lon1 < -180.0)
	lon1 += 360.0;
    while (lon2 > 180.0)
	lon2 -= 360.0;
    while (lon2 < -180.0)
	lon2 += 360.0;
    if (lon1 > lon2)
    {
	t = lon1;
	lon1 = lon2;
	lon2 = lon1;

	t = lat1;
	lat1 = lat2;
	lat2 = t;
    }
*/
    lat1=Radians(lat1);
    lon1=Radians(lon1);
    lat2=Radians(lat2);
    lon2=Radians(lon2);

    t1r=atan(boa*tan(lat1));
    t2r=atan(boa*tan(lat2));

    tm  = (t1r+t2r)/2;
    dtm = (t2r-t1r)/2;

    stm = sin(tm);
    ctm = cos(tm);
    sdtm = sin(dtm);
    cdtm = cos(dtm);

    kl = stm*cdtm;
    kk = sdtm*ctm;

    sdlmr=sin((lon2-lon1)/2);
    l=sdtm*sdtm+sdlmr*sdlmr*(cdtm*cdtm-stm*stm);
    cd=1-2*l;
    dl=acos(cd);
    sd=sin(dl);
    t=dl/sd;

    u=2*kl*kl/(1-l);
    v=2*kk*kk/l;
    d=4*t*t;
    x=u+v;
    e=-2*cd;
    y=u-v;
    a=-d*e;

    return ( al * sd *
		( t - f/4 * (t*x-y) +
		  ff64 *
		  (
		    x * ( a + (t - (a+e)/2) * x) +
		    y* (-2 * d + e*y)
		    + d*x*y
		  )
		)
	    );
}
