/* pj08z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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
/* EQUIDISTANT CONIC  */
/**********************************************************************/

int pj08z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double one = 1.0;
    double epsln = 1e-10;
    static int switch_ = 0;
    double d_1;
    char angs[16 * 4];
    static double a, b, e, x, y;
    double theta;
    static double e0, e1, e2, e3, x0, y0;
    static double gl, es;
    static double ml, rh, ns, cosphi, sinphi, ml0, ml1, ml2, rh0, ms1, ms2;
    int ind;
    static double con, lat0, lat1, lat2, lon0;

    /* PARAMETERS **
     * A,E,ES,LAT1,LAT2,LON0,LAT0,X0,Y0,E0,E1,E2,E3,NS,GL,RH0 */
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
        goto L_is08z0;
    case 2:
        goto L_pf08z0;
    case 3:
        goto L_pi08z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is08z0:
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
    unitz0_(&data[3], &c__5, &lat1, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[4], &c__5, &lat2, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    if ((d_1 = lat1 + lat2, fabs(d_1)) >= epsln)
        goto L130;
    if (ipfile != NULL) {
        fprintf(ipfile, "pj08z0_0_: Equal latitudes for st. parallels");
    fprintf(ipfile, " on opposite sides of equator\n");
        }
    *iflg = 801;
    return 0;
  L130:
    unitz0_(&data[5], &c__5, &lon0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[6], &c__5, &lat0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    x0 = data[7];
    y0 = data[8];
    sinphi = sin(lat1);
    cosphi = cos(lat1);
    ms1 = msfnz0_(&e, &sinphi, &cosphi);
    ml1 = mlfnz0_(&e0, &e1, &e2, &e3, &lat1);
    ind = 0;
    if (data[9] != 0.0)
        goto L140;
    ns = sinphi;
    goto L160;
  L140:
    ind = 1;
    sinphi = sin(lat2);
    cosphi = cos(lat2);
    ms2 = msfnz0_(&e, &sinphi, &cosphi);
    ml2 = mlfnz0_(&e0, &e1, &e2, &e3, &lat2);
    if ((d_1 = lat1 - lat2, fabs(d_1)) >= epsln)
        goto L150;
    ns = sinphi;
    goto L160;
  L150:
    ns = (ms1 - ms2) / (ml2 - ml1);
  L160:
    gl = ml1 + ms1 / ns;
    ml0 = mlfnz0_(&e0, &e1, &e2, &e3, &lat0);
    rh0 = a * (gl - ml0);

    /* LIST RESULTS OF PARAMETER INITIALIZATION. */
    dmslz0_(&lat1, &c__0, angs, ipfile, iflg);
    dmslz0_(&lat2, &c__0, angs + 16, ipfile, iflg);
    dmslz0_(&lon0, &c__0, angs + 32, ipfile, iflg);
    dmslz0_(&lat0, &c__0, angs + 48, ipfile, iflg);
    if (ind == 0)
        goto L200;
    if (ipfile != NULL) {
        fprintf(ipfile,
                "Initialization parameters (equidistant conic projection)");
        fprintf(ipfile, "\tSemi-major axis of ellipsoid = %16.4f meters\n", a);
        fprintf(ipfile, "\tEccentricity squared         = %16.13f\n", es);
        fprintf(ipfile, "\tLatitude of 1st st. parallel = %16s\n", angs);
        fprintf(ipfile, "\tLatitude of 2nd st. parallel = %16s\n", angs + 16);
        fprintf(ipfile, "\tLongitude of origin          = %16s\n", angs + 32);
        fprintf(ipfile, "\tLatitude of origin           = %16s\n", angs + 48);
        fprintf(ipfile, "\tFalse easting                = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %16.4f meters\n", y0);
    }
    goto L220;
  L200:
    if (ipfile != NULL) {
        fprintf(ipfile,
                "Initialization parameters (equidistant conic projection):\n");
        fprintf(ipfile, "\tSemi-major axis of ellipsoid = %16.4f meters\n", a);
        fprintf(ipfile, "\tEccentricity squared         = %16.13f\n", es);
        fprintf(ipfile, "\tLatitude of st. parallel     = %16s\n", angs);
        fprintf(ipfile, "\tLongitude of origin          = %16s\n", angs + 32);
        fprintf(ipfile, "\tLatitude of origin           = %16s\n", angs + 48);
        fprintf(ipfile, "\tFalse easting                = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %16.4f meters\n", y0);
    }
  L220:
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf08z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L300;
    if (ipfile != NULL)
        fprintf(ipfile, "pj08z0_0_: Uninitialized transformation\n");
    *iflg = 800;
    return 0;
  L300:
    ml = mlfnz0_(&e0, &e1, &e2, &e3, &geog[2]);
    rh = a * (gl - ml);
    d_1 = geog[1] - lon0;
    theta = ns * adjlz0_(&d_1);
    proj[1] = x0 + rh * sin(theta);
    proj[2] = y0 + rh0 - rh * cos(theta);
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi08z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L320;
    if (ipfile != NULL)
        fprintf(ipfile, "pj08z0_0_: Uninitialized transformation\n");
    *iflg = 800;
    return 0;
  L320:
    x = proj[1] - x0;
    y = rh0 - proj[2] + y0;
    d_1 = sqrt(x * x + y * y);
    rh = d_sign(&d_1, &ns);
    theta = 0.0;
    con = d_sign(&one, &ns);
    if (rh != 0.0)
        theta = atan2(con * x, con * y);
    ml = gl - rh / a;
    geog[2] = phi3z0_(&ml, &e0, &e1, &e2, &e3, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    d_1 = lon0 + theta / ns;
    geog[1] = adjlz0_(&d_1);
    return 0;
}                               /* pj08z0_ */


int pj08z0_()
{
    return pj08z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is08z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj08z0_0_(1, zone, data, ipfile, iflg, (double*)0, (double*)0);
}

int pf08z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj08z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi08z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj08z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj08z0.c ***/
