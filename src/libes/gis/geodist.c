#include "pi.h"

/* WARNING: this code is preliminary and may be changed,
 * including calling sequences to any of the functions
 * defined here
 */

/* distance from point to point along a geodesic 
 * code from
 *   Paul D. Thomas
 *   "Spheroidal Geodesics, Reference Systems, and Local Geometry"
 *   U.S. Naval Oceanographic Office, p. 162
 *   Engineering Library 526.3 T36s
 */

static double boa;
static double f;
static double ff64;
static double al;

extern double sqrt();
extern double sin(), cos(), tan(), atan(), acos();

/* must be called once to establish the ellipsoid */
G_begin_geodesic_distance (a, e2)
    double a, e2;
{
    al = a;
    boa = sqrt (1 - e2);
    f = 1 - boa;
    ff64 = f*f/64;
}

static double t1,t2,t3,t4,t1r,t2r;

/* must be called first */
G_set_geodesic_distance_lat1(lat1)
    double lat1;
{
    t1r = atan(boa*tan(Radians(lat1)));
}
/* must be called second */
G_set_geodesic_distance_lat2(lat2)
    double lat2;
{
    double stm,ctm,sdtm,cdtm;
    double tm, dtm;

    t2r = atan(boa*tan(Radians(lat2)));

    tm  = (t1r+t2r)/2;
    dtm = (t2r-t1r)/2;

    stm = sin(tm);
    ctm = cos(tm);
    sdtm = sin(dtm);
    cdtm = cos(dtm);

    t1 = stm*cdtm;
    t1 = t1 * t1 * 2;

    t2 = sdtm*ctm;
    t2 = t2 * t2 * 2;

    t3 = sdtm*sdtm;
    t4 = cdtm*cdtm-stm*stm;
}

double
G_geodesic_distance_lon_to_lon (lon1, lon2)
    double lon1, lon2;
{
    double a, cd, d, e, /*dl,*/
	   q, sd, sdlmr, 
	   t, u, v, x, y;


    sdlmr = sin(Radians(lon2-lon1)/2);

    /* special case - shapiro */
    if (sdlmr == 0.0 && t1r == t2r) return 0.0;

    q = t3+sdlmr*sdlmr*t4;

    /* special case - shapiro */
    if (q == 1.0) return PI *al;

/* Mod: shapiro
 * cd=1-2q is ill-conditioned if q is small O(10**-23)
 *   (for high lats? with lon1-lon2 < .25 degrees?)
 *   the computation of cd = 1-2*q will give cd==1.0.
 * However, note that t=dl/sd is dl/sin(dl) which approaches 1 as dl->0.
 * So the first step is to compute a good value for sd without using sin()
 *   and then check cd && q to see if we got cd==1.0 when we shouldn't.
 * Note that dl isn't used except to get t,
 *   but both cd and sd are used later
 */

/* original code
    cd=1-2*q;
    dl=acos(cd);
    sd=sin(dl);
    t=dl/sd;
*/

    cd = 1-2*q;                 /* ill-conditioned subtraction for small q */
/* mod starts here */
    sd = 2* sqrt (q - q * q);	/* sd^2 = 1 - cd^2 */
    if (q != 0.0 && cd == 1.0)  /* test for small q */
	t = 1.0;
    else if (sd == 0.0)
	t = 1.0;
    else
	t = acos(cd)/sd;          /* don't know how to fix acos(1-2*q) yet */
/* mod ends here */

    u = t1/(1-q);
    v = t2/q;
    d = 4*t*t;
    x = u+v;
    e = -2*cd;
    y = u-v;
    a = -d*e;

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

double
G_geodesic_distance (lon1, lat1, lon2, lat2)
    double lon1, lat1, lon2, lat2;
{
    G_set_geodesic_distance_lat1 (lat1);
    G_set_geodesic_distance_lat2 (lat2);
    return G_geodesic_distance_lon_to_lon (lon1, lon2);
}
