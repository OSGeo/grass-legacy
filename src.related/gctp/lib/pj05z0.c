/* pj05z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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
/* MERCATOR  */
/**********************************************************************/

int pj05z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
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
    double ts, sinphi;
    static double a, b, e, es, x, y, m1, x0, y0, lat1, lon0;

    /**** PARAMETERS **** A,E,ES,LON0,X0,Y0,NS,F,RH0,LAT1,M1 **********/
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
        goto L_is05z0;
    case 2:
        goto L_pf05z0;
    case 3:
        goto L_pi05z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */

  L_is05z0:

    *iflg = 0;
    if (switch_ != 0 && switch_ == *zone)
        return 0;
    if (data[1] <= 0.0)
        goto L100;
    a = data[1];
    b = data[2];
    if (b > 0.0)
        goto L40;
    e = 0.0;
    es = 0.0;
    goto L120;
  L40:
    if (b > 1.0)
        goto L60;
    e = sqrt(b);
    es = b;
    goto L120;
  L60:
    /* Computing 2nd power */
    d_1 = b / a;
    es = 1.0 - d_1 * d_1;
    e = sqrt(es);
    goto L120;
  L100:
    a = ellpz0.az;
    e = ellpz0.ez;
    es = ellpz0.esz;
  L120:
    unitz0_(&data[5], &c__5, &lon0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[6], &c__5, &lat1, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    /* Computing 2nd power */
    d_1 = sin(lat1);
    m1 = cos(lat1) / sqrt(1.0 - es * (d_1 * d_1));
    x0 = data[7];
    y0 = data[8];

    /* LIST RESULTS OF PARAMETER INITIALIZATION. */

    dmslz0_(&lat1, &c__0, angs, ipfile, iflg);
    dmslz0_(&lon0, &c__0, angs + 16, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile, "Initialization parameters (mercator projection):\n");
        fprintf(ipfile, "\tSemi-major axis of ellipsoid = %16.4f meters\n", a);
        fprintf(ipfile, "\tEccentricity squared         = %16.13f\n", es);
        fprintf(ipfile, "\tLatitude of true scale       = %16s\n", angs);
        fprintf(ipfile, "\tCentral longitude            = %16s\n", angs + 16);
        fprintf(ipfile, "\tFalse easting                = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */

  L_pf05z0:

    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj05z0_0_: Uninitialized transformation\n");
    *iflg = 500;
    return 0;
  L220:
    if ((d_1 = fabs(geog[2]) - halfpi, fabs(d_1)) > epsln)
        goto L240;
    if (ipfile != NULL)
        fprintf(ipfile, "pj05z0_0_: Transformation cannot be computed at the poles\n");
    *iflg = 501;
    return 0;
  L240:
    sinphi = sin(geog[2]);
    ts = tsfnz0_(&e, &geog[2], &sinphi);
    d_1 = geog[1] - lon0;
    proj[1] = x0 + a * m1 * adjlz0_(&d_1);
    proj[2] = y0 - a * m1 * log(ts);
    return 0;

    /* .  INVERSE TRANSFORMATION  . */

  L_pi05z0:

    *iflg = 0;
    if (switch_ != 0)
        goto L260;
    if (ipfile != NULL)
        fprintf(ipfile, "pj05z0_0_: Uninitialized transformation\n");
    *iflg = 500;
    return 0;
  L260:
    x = proj[1] - x0;
    y = proj[2] - y0;
    ts = exp(-y / (a * m1));
    geog[2] = phi2z0_(&e, &ts, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    d_1 = lon0 + x / (a * m1);
    geog[1] = adjlz0_(&d_1);
    return 0;

}                               /* pj05z0_ */


int pj05z0_()
{
    return pj05z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is05z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj05z0_0_(1, zone, data, ipfile, iflg, (double*)0, (double*)0);
}

int pf05z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj05z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi05z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj05z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj05z0.c ***/
