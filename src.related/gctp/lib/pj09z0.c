/* pj09z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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

/*****************************************************************/
/* NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL  */
/* MATHEMATICAL ANALYSIS BY JOHN SNYDER  * */
/** GCTP/II              VERSION 1.0.2           SEPTEMBER 1,1986  */
/*****************************************************************/
/* TRANSVERSE MERCATOR  */
/******************************************************************/

int pj09z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double halfpi = 1.57079632679489661923;
    double tol = 1e-5;
    double epsln = 1e-10;
    static int switch_ = 0, ind;
    double d_1;
    char angs[16 * 2];
    double dlon, b, c, d, f, g, h, n, r, t, x, y;
    double al, cs, ds;
    double ml, tq, ts, tanphi, cosphi, sinphi;
    double con, lat, als, phi;
    static double a, e, es, ks0, lon0, lat0, x0, y0, e0, e1, e2, e3, esp, ml0;

    /* PARAMETERS *** * A,E,ES,KS0,LON0,LAT0,X0,Y0,E0,E1,E2,E3,ESP,ML0,IND */
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
        goto L_is09z0;
    case 2:
        goto L_pf09z0;
    case 3:
        goto L_pi09z0;
    }

    /* INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is09z0:
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
    e0 = 1.0;
    e1 = 0.0;
    e2 = 0.0;
    e3 = 0.0;
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
    ks0 = data[3];
    unitz0_(&data[5], &c__5, &lon0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[6], &c__5, &lat0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    x0 = data[7];
    y0 = data[8];
    ml0 = a * mlfnz0_(&e0, &e1, &e2, &e3, &lat0);
    ind = 1;
    if (e < tol)
        goto L130;
    ind = 0;
    esp = es / (1.0 - es);

    /* LIST RESULTS OF PARAMETER INITIALIZATION. */
  L130:
    dmslz0_(&lon0, &c__0, angs, ipfile, iflg);
    dmslz0_(&lat0, &c__0, angs + 16, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile,
              "Initialization parameters (transverse mercator projection)\n");
        fprintf(ipfile, "\tEccentricity squared        = %16.13f\n", es);
        fprintf(ipfile, "\tScale factor at c. meridian = %16.4f\n", ks0);
        fprintf(ipfile, "\tLongitude of c. meridian    = %16s\n", angs);
        fprintf(ipfile, "\tLatitude of origin          = %16s\n", angs + 16);
        fprintf(ipfile, "\tFalse easting               = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing              = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf09z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj09z0_0_: Uninitialized transformation\n");
    *iflg = 900;
    return 0;
  L220:
    d_1 = geog[1] - lon0;
    dlon = adjlz0_(&d_1);
    lat = geog[2];
    if (ind == 0)
        goto L240;
    cosphi = cos(lat);
    b = cosphi * sin(dlon);
    if ((d_1 = fabs(b) - 1.0, fabs(d_1)) > epsln)
        goto L230;
    if (ipfile != NULL)
        fprintf(ipfile, "pj09z0_0_: Point projects into infinity\n");
    *iflg = 901;
    return 0;
  L230:
    proj[1] = 0.5 * a * ks0 * log((1.0 + b) / (1.0 - b));
    con = acos(cosphi * cos(dlon) / sqrt(1.0 - b * b));
    if (lat < 0.0)
        con = -con;
    proj[2] = a * ks0 * (con - lat0);
    return 0;

  L240:
    sinphi = sin(lat);
    cosphi = cos(lat);
    al = cosphi * dlon;
    als = al * al;
    c = esp * cosphi * cosphi;
    tq = tan(lat);
    t = tq * tq;
    n = a / sqrt(1.0 - es * sinphi * sinphi);
    ml = a * mlfnz0_(&e0, &e1, &e2, &e3, &lat);
    proj[1] = ks0 * n * al * (1.0 + als / 6.0 * (1.0 - t + c + als / 20. *
              (5.0 - t * 18. + t * t + c * 72. - esp * 58.))) + x0;
    proj[2] = ks0 * (ml - ml0 + n * tq * (als * (0.5 + als / 24. *
              (5.0 - t + 9.0 * c + 4.0 * c * c + als / 30. *
              (61. - t * 58. + t * t + c * 600. - esp * 330.))))) + y0;
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi09z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L320;
    if (ipfile != NULL)
        fprintf(ipfile, "pj09z0_0_: Uninitialized transformation\n");
    *iflg = 900;
    return 0;
  L320:
    x = proj[1] - x0;
    y = proj[2] - y0;
    if (ind == 0)
        goto L340;
    f = exp(x / (a * ks0));
    g = 0.5 * (f - 1.0 / f);
    c = lat0 + y / (a * ks0);
    h = cos(c);
    con = sqrt((1.0 - h * h) / (1.0 + g * g));
    geog[2] = asinz0_(&con);
    if (c < 0.0)
        geog[2] = -geog[2];
    if (g != 0.0 || h != 0.0)
        goto L330;
    geog[1] = lon0;
    return 0;
  L330:
    d_1 = atan2(g, h) + lon0;
    geog[1] = adjlz0_(&d_1);
    return 0;

  L340:
    con = (ml0 + y / ks0) / a;
    phi = phi3z0_(&con, &e0, &e1, &e2, &e3, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    if (fabs(phi) < halfpi)
        goto L400;
    geog[2] = d_sign(&halfpi, &y);
    geog[1] = lon0;
    return 0;
  L400:
    sinphi = sin(phi);
    cosphi = cos(phi);
    tanphi = tan(phi);
    c = esp * cosphi * cosphi;
    cs = c * c;
    t = tanphi * tanphi;
    ts = t * t;
    con = 1.0 - es * sinphi * sinphi;
    n = a / sqrt(con);
    r = n * (1.0 - es) / con;
    d = x / (n * ks0);
    ds = d * d;
    geog[2] = phi - n * tanphi * ds / r * (0.5 - ds / 24. * (5.0 + 3.0 *
              t + 10.0 * c - 4.0 * cs - 9.0 * esp - ds / 30. * (t * 90. + 61.
              + c * 298. + ts * 45. - esp * 252. - 3.0 * cs)));
    d_1 = lon0 + d * (1.0 - ds / 6.0 * (1.0 + 2.0 * t + c - ds / 20. * (5.0
              - 2.0 * c + t * 28. - 3.0 * cs + 8.0 * esp + ts * 24.))) /
            cosphi;
    geog[1] = adjlz0_(&d_1);
    return 0;

} /* pj09z0_ */


int pj09z0_()
{
    return pj09z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is09z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj09z0_0_(1, zone, data, ipfile, iflg,(double*)0,(double*)0);
}

int pf09z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj09z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi09z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj09z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj09z0.c ***/
