/* pj14z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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
/* ORTHOGRAPHIC  */
/**********************************************************************/

int pj14z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double halfpi = 1.57079632679489661923;
    double epsln = 1e-10;
    static int switch_ = 0;
    double d_1;
    char angs[16 * 2];
    static double cosz, sinz, a, g, x, y, z, x0, y0;
    static double cosph0, sinph0;
    static double rh, cosphi, sinphi, coslon, con, lon, ksp, lat0,
     lon0;

    /**** PARAMETERS **** A,LON0,LAT0,X0,Y0,SINPH0,COSPH0 *************/
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
        goto L_is14z0;
    case 2:
        goto L_pf14z0;
    case 3:
        goto L_pi14z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is14z0:
    *iflg = 0;
    if (switch_ != 0 && switch_ == *zone)
        return 0;
    a = data[1];
    if (a <= 0.0)
        a = sphrz0.azz;
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
        fprintf(ipfile,
        "Initialization parameters (orthographic projection):\n");
        fprintf(ipfile, "\tRadius of sphere             = %16.4f meters\n", a);
        fprintf(ipfile, "\tLongitude of center          = %16s\n", angs);
        fprintf(ipfile, "\tLatitude of center           = %16s\n", angs + 16);
        fprintf(ipfile, "\tFalse easting                = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf14z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L120;
    if (ipfile != NULL)
        fprintf(ipfile, "pj14z0_0_: Uninitialized transformation\n");
    *iflg = 1400;
    return 0;
  L120:
    d_1 = geog[1] - lon0;
    lon = adjlz0_(&d_1);
    sinphi = sin(geog[2]);
    cosphi = cos(geog[2]);
    coslon = cos(lon);
    g = sinph0 * sinphi + cosph0 * cosphi * coslon;
    ksp = 1.0;
    if (g > 0.0 || fabs(g) <= epsln)
        goto L140;
    if (ipfile != NULL)
        fprintf(ipfile, "pj14z0_0_: Point cannot be projected\n");
    *iflg = 1401;
    return 0;
  L140:
    proj[1] = x0 + a * ksp * cosphi * sin(lon);
    proj[2] = y0 + a * ksp * (cosph0 * sinphi - sinph0 * cosphi * coslon);
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi14z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj14z0_0_: Uninitialized transformation\n");
    *iflg = 1400;
    return 0;
  L220:
    x = proj[1] - x0;
    y = proj[2] - y0;
    rh = sqrt(x * x + y * y);
    if (rh <= a)
        goto L230;
    if (ipfile != NULL)
        fprintf(ipfile, "pj14z0_0_: Improper parameter\n");
    *iflg = 1402;
    return 0;
  L230:
    d_1 = rh / a;
    z = asinz0_(&d_1);
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
    if (lat0 < 0.0)
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

}                               /* pj14z0_ */


int pj14z0_()
{
    return pj14z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is14z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj14z0_0_(1, zone, data, ipfile, iflg, (double*)0, (double*)0);
}

int pf14z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj14z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi14z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj14z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj14z0.c ***/
