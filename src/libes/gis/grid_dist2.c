#include "pi.h"

/* I think the formula used here is correct */

double
G_ellipsoid_grid_dist (a, e2, lon1, lat1, lon2, lat2)
    double a, e2, lon1, lat1, lon2, lat2;
{
    double cos1, cos2, sin1, sin2;
    double q, q2, m, m2;
    double A,B,C,D;
    double Ap, Bp, Cp, Dp;
    double distance;
    double e4, e6;
    double sin(), cos(), sqrt();
    double sc();

/* adjust longitudes so that they are as close as can be */
    if (lon1 > lon2)
	while ((lon1-lon2) > 180)
	    lon2 += 360;
    else if (lon2 > lon1)
	while ((lon2-lon1) > 180)
	    lon1 += 360;
    
    lon1 = Radians(lon1);
    lon2 = Radians(lon2);
    lat1 = Radians(lat1);
    lat2 = Radians(lat2);

    sin1 = sin(lat1);
    cos1 = cos(lon1);
    sin2 = sin(lat2);
    cos2 = cos(lon2);

    e4 = e2 * e2;
    e6 = e2 * e4;

/* along a parallel */
    if (lat1 == lat2)
    {
	distance = a * cos1 * (lon2 - lon1) / sqrt(1 - e2 * sin1 * sin1);
    }

/* along a meridian */
    else if (lon1 == lon2)
    {
	A = 1 + e2 * (3.0/4.0) + e4 * (45.0/64.0) + e6 * (175.0/256.0);
	B =   - e2 * (3.0/4.0) - e4 * (45.0/64.0) - e6 * (175.0/256.0);
	C =                    - e4 * (15.0/32.0) - e6 * (175.0/384.0);
	D =                                       - e6 * (35.0/96.0);

	distance = a * (1-e2) * (sc(A,B,C,D,lat2,0.,0.)-sc(A,B,C,D,lat1,0.,0.));
    }

/* other lines */
    else
    {
	q  = 1 - e2;
	q2 = q*q;

	m = (lat2-lat1)/(lon2-lon1);
	m2 = m*m;

	A  = sqrt(q2 + m2);
	B  = (3.0*e2*q2 - q*m2)/(2.0*A);
	C  = (6.0*e4*q2 - e2*q*m2 - B*B)/(2.0*A);
	D  = (10.0*e6*q2 - e4*q*m2 - 2.0*B*C)/(2.0*A);

	Ap = A + B/2.0 + 3.0*C/8.0 + 5.0*D/16.0;
	Bp =   - B/2.0 - 3.0*C/8.0 - 5.0*D/16.0;
	Cp =           -     C/4.0 - 5.0*D/24.0;
	Dp =                       -     D/6.0;

	distance = a *
	    (sc(Ap,Bp,Cp,Dp,lat2,sin2,cos2)-sc(Ap,Bp,Cp,Dp,lat1,sin1,cos1));
    }

    if (distance < 0.0)
	distance = -distance;
    return distance;
}

static double sc(A,B,C,D,lat,s,c)
    double A,B,C,D,lat,s,c;
{
    double s2;

    s2 = s*s;

    return A * lat + c*s*(B + s2 * (C + s2*D));
}
