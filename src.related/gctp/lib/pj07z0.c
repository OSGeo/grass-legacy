/* pj07z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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
/* POLYCONIC  */
/**********************************************************************/
int pj07z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    static double tol = 1e-7;
    static double zero = 0.;
    static double one = 1.;
    static int switch_ = 0;
    double d_1, d_2;
    char angs[16 * 2];
    double b, c, x, y, al;
    double ml, ms, cosphi, sinphi, con;
    static double a, e, es, lon0, lat0, x0, y0, e0, e1, e2, e3, ml0;

    /**** PARAMETERS **** A,E,ES,LON0,LAT0,X0,Y0,E0,E1,E2,E3,ML0 **********/
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
        goto L_is07z0;
    case 2:
        goto L_pf07z0;
    case 3:
        goto L_pi07z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is07z0:

    *iflg = 0;
    if (switch_ != 0 && switch_ == *zone)
        return 0;
    if (data[1] <= zero)
        goto L100;
    a = data[1];
    b = data[2];
    if (b > zero)
        goto L40;
    e = zero;
    es = zero;
    e0 = one;
    e1 = zero;
    e2 = zero;
    e3 = zero;
    goto L120;
  L40:
    if (b > one)
        goto L60;
    e = sqrt(b);
    es = b;
    goto L80;
  L60:
    /* Computing 2nd power */
    d_1 = b / a;
    es = one - d_1 * d_1;
    e = sqrt(es);
  L80:
    e0 = e0fnz0_(&es);
    e1 = e1fnz0_(&es);
    e2 = e2fnz0_(&es);
    e3 = e3fnz0_(&es);
    goto L120;
  L100:
    a = ellpz0.az;
    e = ellpz0.ez;
    es = ellpz0.esz;
    e0 = ellpz0.e0z;
    e1 = ellpz0.e1z;
    e2 = ellpz0.e2z;
    e3 = ellpz0.e3z;
  L120:
    unitz0_(&data[5], &c__5, &lon0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[6], &c__5, &lat0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    x0 = data[7];
    y0 = data[8];
    ml0 = mlfnz0_(&e0, &e1, &e2, &e3, &lat0);

    /* LIST RESULTS OF PARAMETER INITIALIZATION. */
    dmslz0_(&lon0, &c__0, angs, ipfile, iflg);
    dmslz0_(&lat0, &c__0, angs + 16, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile, "Initialization parameters (polyconic projection):\n");
        fprintf(ipfile, "\tSemi-major axis of ellipsoid = %16.4f meters\n", a);
        fprintf(ipfile, "\tEccentricity squared         = %16.13f\n", es);
        fprintf(ipfile, "\tLongitude of origin          = %16s\n", angs);
        fprintf(ipfile, "\tLatitude of origin           = %16s\n", angs + 16);
        fprintf(ipfile, "\tFalse easting                = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf07z0:

    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj07z0_0_: Uninitialized transformation\n");
    *iflg = 700;
    return 0;
  L220:
    d_1 = geog[1] - lon0;
    con = adjlz0_(&d_1);
    if (fabs(geog[2]) > tol)
        goto L240;
    proj[1] = x0 + a * con;
    proj[2] = y0 - a * ml0;
    return 0;
  L240:
    sinphi = sin(geog[2]);
    cosphi = cos(geog[2]);
    ml = mlfnz0_(&e0, &e1, &e2, &e3, &geog[2]);
    ms = msfnz0_(&e, &sinphi, &cosphi);
    con *= sinphi;
    proj[1] = x0 + a * ms * sin(con) / sinphi;
    proj[2] = y0 + a * (ml - ml0 + ms * (one - cos(con)) / sinphi);
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi07z0:

    *iflg = 0;
    if (switch_ != 0)
        goto L320;
    if (ipfile != NULL)
        fprintf(ipfile, "pj07z0_0_: Uninitialized transformation\n");
    *iflg = 700;
    return 0;
  L320:
    x = proj[1] - x0;
    y = proj[2] - y0;
    al = ml0 + y / a;
    if (fabs(al) > tol)
        goto L340;
    geog[1] = x / a + lon0;
    geog[2] = zero;
    return 0;
  L340:
    /* Computing 2nd power */
    d_1 = x / a;
    b = al * al + d_1 * d_1;
    geog[2] = phi4z0_(&es, &e0, &e1, &e2, &e3, &al, &b, &c, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    d_2 = x * c / a;
    d_1 = asinz0_(&d_2) / sin(geog[2]) + lon0;
    geog[1] = adjlz0_(&d_1);
    return 0;

}                               /* pj07z0_ */

int pj07z0_()
{
    return pj07z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is07z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj07z0_0_(1, zone, data, ipfile, iflg,
            (double*)0, (double*)0);
}

int pf07z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj07z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi07z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj07z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}
/*** end pj07z0.c ***/
