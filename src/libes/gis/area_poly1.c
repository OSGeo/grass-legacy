#include "pi.h"

static double QA, QB, QC;
static double QbarA, QbarB, QbarC, QbarD;
static double AE;  /* a^2(1-e^2) */
static double Qp;  /* Q at the north pole */
static double E;   /* area of the earth */
static double TwoPI;

double sin(), cos();

static double Q(x)
    double x;
{
    double sinx, sinx2;

    sinx = sin(x);
    sinx2 = sinx * sinx;

    return sinx * (1 + sinx2 * (QA + sinx2 * (QB + sinx2 * QC)));
}
static double Qbar(x)
    double x;
{
    double cosx, cosx2;

    cosx = cos(x);
    cosx2 = cosx * cosx;

    return cosx * (QbarA + cosx2 * (QbarB + cosx2 * (QbarC + cosx2 * QbarD)));
}

G_begin_ellipsoid_polygon_area (a, e2)
    double a, e2;
{
    double e4, e6;

    TwoPI = PI+PI;

    e4 = e2 * e2;
    e6 = e4 * e2;

    AE = a * a * (1 - e2);

    QA = (2.0/3.0)*e2;
    QB = (3.0/5.0)*e4;
    QC = (4.0/7.0)*e6;

    QbarA = -1.0 - (2.0/3.0)*e2 - (3.0/5.0)*e4  -  (4.0/7.0)*e6;
    QbarB =        (2.0/9.0)*e2 + (2.0/5.0)*e4  +  (4.0/7.0)*e6;
    QbarC =                     - (3.0/25.0)*e4 - (12.0/35.0)*e6;
    QbarD =                                        (4.0/49.0)*e6;

    Qp = Q(PI/2);
    E  = 4 * PI * Qp * AE;
    if (E < 0.0) E = -E;
}

double
G_ellipsoid_polygon_area (lon, lat, n)
    double *lon, *lat;
{
    double x1,y1,x2,y2,dx,dy;
    double Qbar1, Qbar2;
    double area;

    x2 = Radians (lon[n-1]);
    y2 = Radians (lat[n-1]);
    Qbar2 = Qbar(y2);

    area = 0.0;

    while (--n >= 0)
    {
	x1 = x2;
	y1 = y2;
	Qbar1 = Qbar2;

	x2 = Radians (*lon++);
	y2 = Radians (*lat++);
	Qbar2 = Qbar(y2);

	if (x1 > x2)
	    while (x1 - x2 > PI)
		x2 += TwoPI;
	else if (x2 > x1)
	    while (x2 - x1 > PI)
		x1 += TwoPI;

	dx = x2 - x1;
	area += dx * (Qp - Q(y2));

	if ((dy = y2 - y1) != 0.0)
	    area += dx * Q(y2) - (dx/dy)*(Qbar2-Qbar1);
    }
    if((area *= AE) < 0.0)
	area = -area;

    /* kludge - if polygon circles the south pole the area will be
     * computed as if it cirlced the north pole. The correction is
     * the difference between total surface area of the earth and
     * the "north pole" area.
     */
    if (area > E) area = E;
    if (area > E/2) area = E - area;

    return area;
}
