/* pj06z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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
/* POLAR STEREOGRAPHIC  */
/**********************************************************************/

int pj06z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double nintyd = 9e5;
    static int switch_ = 0;
    double d_1;
    char angs[16 * 2];
    static double latc;
    static double save, a, b, e, x, y;
    static double e4, x0, y0;
    static double es;
    static double rh, ts, cosphi, sinphi, fac;
    static int ind;
    static double mcs, tcs, con1, con2, lon0;

    /**** PARAMETERS **** A,E,ES,LON0,LATC,X0,Y0,E4,MCS,TCS,FAC,IND ***/
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
        goto L_is06z0;
    case 2:
        goto L_pf06z0;
    case 3:
        goto L_pi06z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */

  L_is06z0:
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
    e4 = 1.0;
    goto L120;
  L40:
    if (b > 1.0)
        goto L60;
    e = sqrt(b);
    es = b;
    goto L80;
  L60:
    /* Computing 2nd power */
    d_1 = b / a;
    es = 1.0 - d_1 * d_1;
    e = sqrt(es);
  L80:
    e4 = e4fnz0_(&e);
    goto L120;
  L100:
    a = ellpz0.az;
    e = ellpz0.ez;
    es = ellpz0.esz;
    e4 = ellpz0.e4z;
  L120:
    unitz0_(&data[5], &c__5, &lon0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    save = data[6];
    unitz0_(&save, &c__5, &latc, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    x0 = data[7];
    y0 = data[8];
    fac = 1.0;
    if (save < 0.0)
        fac = -1.0;
    ind = 0;
    if (fabs(save) == nintyd)
        goto L130;
    ind = 1;
    con1 = fac * latc;
    sinphi = sin(con1);
    cosphi = cos(con1);
    mcs = msfnz0_(&e, &sinphi, &cosphi);
    tcs = tsfnz0_(&e, &con1, &sinphi);

    /* LIST RESULTS OF PARAMETER INITIALIZATION. */
  L130:
    dmslz0_(&lon0, &c__0, angs, ipfile, iflg);
    dmslz0_(&latc, &c__0, angs + 16, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile,
                "Initialization parameters (polar stereographic projection):\n");
        fprintf(ipfile, "\tSemi-major axis of ellipsoid = %16.4f meters\n", a);
        fprintf(ipfile, "\tEccentricity squared         = %16.13f\n", es);
        fprintf(ipfile, "\tLongitude of y - axis        = %16s\n", angs);
        fprintf(ipfile, "\tLatitude of true scale       = %16s\n", angs + 16);
        fprintf(ipfile, "\tFalse easting                = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf06z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj06z0_0_: Uninitialized transformation\n");
    *iflg = 600;
    return 0;
  L220:
    d_1 = geog[1] - lon0;
    con1 = fac * adjlz0_(&d_1);
    con2 = fac * geog[2];
    sinphi = sin(con2);
    ts = tsfnz0_(&e, &con2, &sinphi);
    if (ind == 0)
        goto L240;
    rh = a * mcs * ts / tcs;
    goto L260;
  L240:
    rh = 2.0 * a * ts / e4;
  L260:
    proj[1] = x0 + fac * rh * sin(con1);
    proj[2] = y0 - fac * rh * cos(con1);
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi06z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L320;
    if (ipfile != NULL)
        fprintf(ipfile, "pj06z0_0_: Uninitialized transformation\n");
    *iflg = 600;
    return 0;
  L320:
    x = fac * (proj[1] - x0);
    y = fac * (proj[2] - y0);
    rh = sqrt(x * x + y * y);
    if (ind == 0)
        goto L340;
    ts = rh * tcs / (a * mcs);
    goto L360;
  L340:
    ts = rh * e4 / (2.0 * a);
  L360:
    geog[2] = fac * phi2z0_(&e, &ts, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    if (rh != 0.0)
        goto L400;
    geog[1] = fac * lon0;
    return 0;
  L400:
    d_1 = fac * atan2(x, -y) + lon0;
    geog[1] = adjlz0_(&d_1);
    return 0;
}                               /* pj06z0_ */


int pj06z0_()
{
    return pj06z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is06z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj06z0_0_(1, zone, data, ipfile, iflg, (double*)0, (double*)0);
}

int pf06z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj06z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi06z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj06z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end  pj06z0.c ***/
