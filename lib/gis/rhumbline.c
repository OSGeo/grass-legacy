/*
 * This code is preliminary. I don't know if it is even
 * correct.
 */

/*
 * From "Map Projections" by Peter Richardus and Ron K. Alder, 1972
 * (526.8 R39m in Map & Geography Library)
 * page  20,21, formulas 2.21,2.22
 *
 * Formula is the equation of a rhumbline from (lat1,lon1) to (lat2,lon2)
 * Input is lon, output is lat (all in degrees)
 *
 * Note formula only works if 0 < abs(lon2-lon1) < 180
 * If lon1 == lon2 then rhumbline is the merdian lon1 
 * (and the formula will fail)
 */

#include <math.h>
#include <grass/gis.h>
#include "pi.h"


static int adjust_lat(double *);
#if 0
static int adjust_lon(double *);
#endif /* unused */

static double TAN_A, TAN1, TAN2, L;
static int parallel;


int G_begin_rhumbline_equation (
    double lon1,double lat1,double lon2,double lat2)
{
    adjust_lat (&lat1);
    adjust_lat (&lat2);

    if (lon1 == lon2)
    {
	parallel = 1;	/* a lie */
	L = lat1;
	return 0;
    }
    if (lat1 == lat2)
    {
	parallel = 1;
	L = lat1;
	return 1;
    }
    parallel = 0;
    lon1 = Radians(lon1);
    lon2 = Radians(lon2);
    lat1 = Radians(lat1);
    lat2 = Radians(lat2);

    TAN1 = tan (PI/4 + lat1/2.0);
    TAN2 = tan (PI/4 + lat2/2.0);
    TAN_A = (lon2 - lon1) / (log(TAN2) - log(TAN1));
    L    = lon1;

    return 1;
}

/* only works if lon1 < lon < lon2 */

double G_rhumbline_lat_from_lon (double lon)
{
    if (parallel) return L;

    lon = Radians(lon);

    return Degrees (2 * atan(exp((lon-L)/TAN_A) * TAN1) - PI/2.0) ;
}

#if 0
static int adjust_lon(double *lon)
{
    while (*lon > 180.0)
	*lon -= 360.0;
    while (*lon < -180.0)
	*lon += 360.0;

    return 0;
}
#endif /* unused */

static int adjust_lat(double *lat)
{
    if (*lat >  90.0) *lat =  90.0;
    if (*lat < -90.0) *lat = -90.0;

    return 0;
}
