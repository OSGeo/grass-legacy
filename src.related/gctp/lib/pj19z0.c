/* pj19z0.f -- translated by f2c (version of 11 May 1990  14:38:39). 
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
/* VAN DER GRINTEN I  */
/**********************************************************************/

int pj19z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double pi = 3.14159265358979323846;
    static int switch_ = 0;
    double halfpi = 1.57079632679489661923;
    double epsln = 1e-10;
    double tol = .7;
    int nit = 35;
    int i_1;
    double d_1, d_2;
    char angs[16];
    static double a, lon0, x0, y0;
    double d, g, h;
    int i;
    double j, m, x, y, theta, y1;
    double dphi;
    double al;
    double costht, sintht, cmm, cnn, con, lat, com, phi, asq, lon, gsq, msq;

    /**** PARAMETERS **** A,LON0,X0,Y0 ********************************/
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
        goto L_is19z0;
    case 2:
        goto L_pf19z0;
    case 3:
        goto L_pi19z0;
    }

  L_is19z0:
    *iflg = 0;
    if (switch_ != 0 && switch_ == *zone)
        return 0;
    a = data[1];
    if (a <= 0.0)
        a = sphrz0.azz;
    unitz0_(&data[5], &c__5, &lon0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    x0 = data[7];
    y0 = data[8];

    /* LIST RESULTS OF PARAMETER INITIALIZATION. */

    dmslz0_(&lon0, &c__0, angs, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile,
                "Initialization parameters (VAN DER GRINTEN 1 projection)\n");
        fprintf(ipfile, "\tRadius of sphere         = %16.4f meters\n", a);
        fprintf(ipfile, "\tLongitude of c. meridian = %16s\n", angs);
        fprintf(ipfile, "\tFalse easting  = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf19z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L120;
    if (ipfile != NULL)
        fprintf(ipfile, "pj19z0_0_: Uninitialized transformation\n");
    *iflg = 1900;
    return 0;
  L120:
    d_1 = geog[1] - lon0;
    lon = adjlz0_(&d_1);
    lat = geog[2];
    if (fabs(lat) > epsln)
        goto L140;
    proj[1] = x0 + a * lon;
    proj[2] = y0;
    return 0;
  L140:
    d_2 = (d_1 = lat / halfpi, fabs(d_1));
    theta = asinz0_(&d_2);
    if (fabs(lon) > epsln && (d_1 = fabs(lat) - halfpi, fabs(d_1)) > epsln)
        goto L160;
    proj[1] = x0;
    d_1 = tan(0.5 * theta);
    proj[2] = y0 + pi * a * d_sign(&d_1, &lat);
    return 0;
  L160:
    al = 0.5 * (d_1 = pi / lon - lon / pi, fabs(d_1));
    asq = al * al;
    sintht = sin(theta);
    costht = cos(theta);
    g = costht / (sintht + costht - 1.0);
    gsq = g * g;
    m = g * (2.0 / sintht - 1.0);
    msq = m * m;
    /* Computing 2nd power */
    d_1 = g - msq;
    con = pi * a * (al * (g - msq) + sqrt(asq * (d_1 * d_1) -
                    (msq + asq) * (gsq - msq))) / (msq + asq);
    con = d_sign(&con, &lon);
    proj[1] = x0 + con;
    con = (d_1 = con / (pi * a), fabs(d_1));
    d_1 = pi * a * sqrt(1.0 - con * con - 2.0 * al * con);
    proj[2] = y0 + d_sign(&d_1, &lat);
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi19z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj19z0_0_: Uninitialized transformation\n");
    *iflg = 1900;
    return 0;
  L220:
    x = proj[1] - x0;
    y = proj[2] - y0;
    con = (d_1 = y / (pi * a), fabs(d_1));
    theta = 2.0 * atan(con);
    if (fabs(x) > epsln)
        goto L240;
    geog[1] = lon0;
    d_1 = sin(theta);
    geog[2] = halfpi * d_sign(&d_1, &y);
    return 0;
  L240:
    if (fabs(y) > epsln)
        goto L260;
    d_1 = lon0 + x / a;
    geog[1] = adjlz0_(&d_1);
    geog[2] = 0.0;
    return 0;
  L260:
    if (sqrt(x * x + y * y) <= pi * a) {
        goto L270;
    }
    if (ipfile != NULL)
        fprintf(ipfile, "pj19z0_0_: Improper parameter\n");
    *iflg = 1901;
    return 0;
  L270:
    cnn = con * con;
    com = (d_1 = x / (pi * a), fabs(d_1));
    cmm = com * com;
    al = (1.0 - cmm - cnn) / (2.0 * com);
    d_2 = pi * (-al + sqrt(al * al + 1.0));
    d_1 = lon0 + d_sign(&d_2, &x);
    geog[1] = adjlz0_(&d_1);
    phi = theta;
    if (con > tol) {
        goto L320;
    }
    /* LOW LATITUDE CASE */
    i_1 = nit;
    for (i = 1; i <= i_1; ++i) {
        d_1 = phi / halfpi;
        theta = asinz0_(&d_1);
        sintht = sin(theta);
        costht = cos(theta);
        g = costht / (sintht + costht - 1.0);
        d = con / sintht - 1.0 / (1.0 + costht);
        h = 2.0 - sintht;
        j = tan(0.5 * theta);
        dphi = (cmm + cnn - 2.0 * d * g * h - j * j) * pi * costht / (4.0 * (
               g * h * (con * costht / (1.0 - costht) + j) / (1.0 + costht)
               + d * g * ((1.0 + 2.0 * costht * costht) / costht + h * (
               costht - sintht) / (sintht + costht - 1.0)) - j * (j * j +
               1.0)));
        phi -= dphi;
        if (fabs(dphi) < epsln) {
            goto L400;
        }
        /* L280: */
    }
  L300:
    if (ipfile != NULL)
        fprintf(ipfile, "pj19z0_0_: Latitude failed to converge\n");
    *iflg = 1902;
    return 0;

    /* HIGH LATITUDE CASE. */
  L320:
    d_1 = geog[1] - lon0;
    lon = adjlz0_(&d_1);
    i_1 = nit;
    for (i = 1; i <= i_1; ++i) {
        if (fabs(phi) > epsln)
            goto L330;
        y1 = 0.0;
        goto L360;
      L330:
        d_2 = (d_1 = phi / halfpi, fabs(d_1));
        theta = asinz0_(&d_2);
        if (fabs(lon) > epsln)
            goto L340;
        y1 = pi * a * tan(0.5 * theta);
        goto L360;
      L340:
        al = 0.5 * (d_1 = pi / lon - lon / pi, fabs(d_1));
        asq = al * al;
        sintht = sin(theta);
        costht = cos(theta);
        g = costht / (sintht + costht - 1.0);
        gsq = g * g;
        m = g * (2.0 / sintht - 1.0);
        msq = m * m;
        /* Computing 2nd power */
        d_2 = g - msq;
        con = (d_1 = (al * (g - msq) + sqrt(asq * (d_2 * d_2) - (msq + asq) *
                                (gsq - msq))) / (msq + asq), fabs(d_1));
        d_1 = pi * a * sqrt(1.0 - con * con - 2.0 * al * con);
        y1 = d_sign(&d_1, &phi);
      L360:
        dphi = (fabs(y) - y1) / (pi * a - y1) * (halfpi - phi);
        phi += dphi;
        if (fabs(dphi) < epsln)
            goto L400;
        /* L380: */
    }
    goto L300;
  L400:
    geog[2] = d_sign(&phi, &y);
    return 0;

}                               /* pj19z0_ */

int pj19z0_()
{
    return pj19z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is19z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj19z0_0_(1, zone, data, ipfile, iflg,(double*)0,(double*)0);
}

int pf19z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj19z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi19z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj19z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj19z0.c ***/
