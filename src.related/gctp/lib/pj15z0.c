/* pj15z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include <math.h>
#include "gctp.h"

/* Table of constant values */
static int c__5 = 5;
static int c__0 = 0;

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/**          MATHEMATICAL ANALYSIS BY JOHN SNYDER  **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/
/* GENERAL VERTICAL NEAR-SIDE PERSPECTIVE  */
/**********************************************************************/

int pj15z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double halfpi = 1.57079632679489661923;
    double epsln = 1e-10;
    double zero = 0.;
    double one = 1.;
    static int switch_ = 0;
    double d_1;
    char angs[16 * 2];
    static double cosz, sinz, a, g, p, r, x, y, z, x0, y0;
    static double cosph0, sinph0;
    static double rh, cosphi, sinphi, coslon, com, con, lon, ksp, lat0, lon0;

    /**** PARAMETERS **** A,P,LON0,LAT0,X0,Y0,SINPH0,COSPH0 ***************/
    /* Parameter adjustments */
    if (data)
        --data;
    if (geog)
        --geog;
    if (proj)
        --proj;
    /* Function Body */
    switch (n__) {
    case 1:
        goto L_is15z0;
    case 2:
        goto L_pf15z0;
    case 3:
        goto L_pi15z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is15z0:

    *iflg = 0;
    if (switch_ != 0 && switch_ == *zone)
        return 0;
    a = data[1];
    if (a <= zero)
        a = sphrz0.azz;
    p = one + data[3] / a;
    unitz0_(&data[5], &c__5, &lon0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[6], &c__5, &lat0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    x0 = data[7];
    y0 = data[8];
    sinph0 = sin(lat0);
    cosph0 = cos(lat0);

    /* LIST RESULTS OF PARAMETER INITIALIZATION. */
    dmslz0_(&lon0, &c__0, angs, ipfile, iflg);
    dmslz0_(&lat0, &c__0, angs + 16, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile, "Initialization parameters ");
    fprintf(ipfile, "(general vertical near-side perspective projection):\n");
        fprintf(ipfile, "\tRadius of sphere             = %16.4f meters\n", a);
        fprintf(ipfile,
                "\tHeight of perspective point above sphere = %16.4f meters\n",
                data[3]);
        fprintf(ipfile, "\tLongitude of center          = %16s\n", angs);
        fprintf(ipfile, "\tLatitude of center           = %16s\n", angs + 16);
        fprintf(ipfile, "\tFalse easting                = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf15z0:

    *iflg = 0;
    if (switch_ != 0)
        goto L120;
    if (ipfile != NULL)
        fprintf(ipfile, "pj15z0_0_: Uninitialized transformation\n");
    *iflg = 1500;
    return 0;
  L120:
    d_1 = geog[1] - lon0;
    lon = adjlz0_(&d_1);
    sinphi = sin(geog[2]);
    cosphi = cos(geog[2]);
    coslon = cos(lon);
    g = sinph0 * sinphi + cosph0 * cosphi * coslon;
    if (g >= one / p)
        goto L140;
    if (ipfile != NULL)
        fprintf(ipfile, "pj15z0_0_: Point cannot be projected\n");
    *iflg = 1501;
    return 0;
  L140:
    ksp = (p - one) / (p - g);
    proj[1] = x0 + a * ksp * cosphi * sin(lon);
    proj[2] = y0 + a * ksp * (cosph0 * sinphi - sinph0 * cosphi * coslon);
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi15z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj15z0_0_: Uninitialized transformation\n");
    *iflg = 1500;
    return 0;
  L220:
    x = proj[1] - x0;
    y = proj[2] - y0;
    rh = sqrt(x * x + y * y);
    r = rh / a;
    con = p - one;
    com = p + one;
    if (r <= sqrt(con / com))
        goto L230;
    if (ipfile != NULL)
        fprintf(ipfile, "pj15z0_0_: Improper parameter\n");
    *iflg = 1502;
    return 0;
  L230:
    sinz = (p - sqrt(one - r * r * com / con)) / (con / r + r / con);
    z = asinz0_(&sinz);
    sinz = sin(z);
    cosz = cos(z);
    geog[1] = lon0;
    if (fabs(rh) > epsln)
        goto L240;
    geog[2] = lat0;
    return 0;
  L240:
    d_1 = cosz * sinph0 + y * sinz * cosph0 / rh;
    geog[2] = asinz0_(&d_1);
    con = fabs(lat0) - halfpi;
    if (fabs(con) > epsln)
        goto L260;
    if (lat0 < zero)
        goto L250;
    d_1 = lon0 + atan2(x, -y);
    geog[1] = adjlz0_(&d_1);
    return 0;
  L250:
    d_1 = lon0 - atan2(-x, y);
    geog[1] = adjlz0_(&d_1);
    return 0;
  L260:
    con = cosz - sinph0 * sin(geog[2]);
    if (fabs(con) <= epsln && fabs(x) <= epsln)
        return 0;
    d_1 = lon0 + atan2(x * sinz * cosph0, con * rh);
    geog[1] = adjlz0_(&d_1);
    return 0;

}                               /* pj15z0_ */

int pj15z0_()
{
    return pj15z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is15z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj15z0_0_(1, zone, data, ipfile, iflg,(double*)0,(double*)0);
}

int pf15z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj15z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi15z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj15z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj15z0.c ***/
