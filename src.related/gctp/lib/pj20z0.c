/* pj20z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include <math.h>
#include "gctp.h"

#define max(a,b) ((a) >= (b) ? (a) : (b))

/* Table of constant values */
static int c__0 = 0;
static int c__5 = 5;

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/**          MATHEMATICAL ANALYSIS BY JOHN SNYDER  **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/
/* OBLIQUE MERCATOR (HOTINE)  */
/**********************************************************************/

int pj20z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double pi = 3.14159265358979323846;
    double halfpi = 1.57079632679489661923;
    double one = 1.0, tol = 1e-7;
    double epsln = 1e-10;
    static int switch_ = 0;
    double d_1, d_2;
    int mode;
    double dlon;
    char angs1[16 * 5], angs2[16 * 3];
    double b, d, f, g, h, j, l, p, q, s, t, x, y;
    double cosph0, sinph0;
    double ul, vl, us, ts, vs, sinphi, ts0, ts1, ts2, com, con, lon;
    static double cosalf, cosgam, singam, sinalf;
    static double a, e, es, ks0, alpha, lonc, lon1, lat1, lon2, lat2, lat0, 
           x0, y0, gamma, lon0, al, bl, el;


    /**** PARAMETERS ****
     * A,E,ES,KS0,ALPHA,LONC,LON1,LAT1,LON2,LAT2,LAT0 **/
    /********************** X0,Y0,GAMMA,LON0,AL,BL,EL *****************/
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
        goto L_is20z0;
    case 2:
        goto L_pf20z0;
    case 3:
        goto L_pi20z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is20z0:
    *iflg = 0;
    if (switch_ != 0 && switch_ == *zone)
        return 0;
    mode = 0;
    if (data[13] != 0.0)
        mode = 1;
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
    ks0 = data[3];
    unitz0_(&data[6], &c__5, &lat0, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    x0 = data[7];
    y0 = data[8];
    sinph0 = sin(lat0);
    cosph0 = cos(lat0);
    con = 1.0 - es * sinph0 * sinph0;
    com = sqrt(1.0 - es);
    /* Computing 4th power */
    d_1 = cosph0, d_1 *= d_1;
    bl = sqrt(1.0 + es * (d_1 * d_1) / (1.0 - es));
    al = a * bl * ks0 * com / con;
    ts0 = tsfnz0_(&e, &lat0, &sinph0);
    con = sqrt(con);
    d = bl * com / (cosph0 * con);
    /* Computing MAX */
    d_2 = d * d - 1.0;
    d_1 = sqrt((max(d_2, 0.)));
    f = d + d_sign(&d_1, &lat0);
    el = f * pow_dd(&ts0, &bl);
    if (ipfile != NULL) {
        fprintf(ipfile, "Initialization parameters");
        fprintf(ipfile, "(oblique mercator 'hotine' projection)\n");
        fprintf(ipfile, "\tSemi-major axis of ellipsoid = %lf meters\n", a);
        fprintf(ipfile, "\tEccentricity squared         = %lf\n", es);
        fprintf(ipfile, "\tScale at center              = %lf\n", ks0);
    }
    if (mode == 0)
        goto L140;
    unitz0_(&data[4], &c__5, &alpha, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[5], &c__5, &lonc, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    g = 0.5 * (f - 1.0 / f);
    d_1 = sin(alpha) / d;
    gamma = asinz0_(&d_1);
    d_1 = g * tan(gamma);
    lon0 = lonc - asinz0_(&d_1) / bl;

    /* LIST INITIALIZATION PARAMETERS (CASE B). */
    dmslz0_(&alpha, &c__0, angs2, ipfile, iflg);
    dmslz0_(&lonc, &c__0, angs2 + 16, ipfile, iflg);
    dmslz0_(&lat0, &c__0, angs2 + 32, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile, "Azimuth of central line        = %lf\n", angs2[0]);
        fprintf(ipfile, "Longitude of origin            = %lf\n", angs2[15]);
        fprintf(ipfile, "Latitude of origin             = %lf\n", angs2[31]);
    }
    con = fabs(lat0);
    if (con > epsln && (d_1 = con - halfpi, fabs(d_1)) > epsln)
        goto L160;
    if (ipfile != NULL)
        fprintf(ipfile, "pj20z0_0_: Improper parameter\n");
    *iflg = 2002;
    return 0;
  L140:
    unitz0_(&data[9], &c__5, &lon1, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[10], &c__5, &lat1, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[11], &c__5, &lon2, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    unitz0_(&data[12], &c__5, &lat2, &c__0, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    sinphi = sin(lat1);
    ts1 = tsfnz0_(&e, &lat1, &sinphi);
    sinphi = sin(lat2);
    ts2 = tsfnz0_(&e, &lat2, &sinphi);
    h = pow_dd(&ts1, &bl);
    l = pow_dd(&ts2, &bl);
    f = el / h;
    g = 0.5 * (f - 1.0 / f);
    j = (el * el - l * h) / (el * el + l * h);
    p = (l - h) / (l + h);
    dmslz0_(&lon2, &c__0, angs1 + 32, ipfile, iflg);
    dlon = lon1 - lon2;
    if (dlon < -pi)
        lon2 -= pi * 2.;
    if (dlon > pi)
        lon2 += pi * 2.;
    dlon = lon1 - lon2;
    lon0 = 0.5 * (lon1 + lon2) - atan(j * tan(0.5 * bl * dlon) / p) / bl;
    d_1 = lon1 - lon0;
    dlon = adjlz0_(&d_1);
    gamma = atan(sin(bl * dlon) / g);
    d_1 = d * sin(gamma);
    alpha = asinz0_(&d_1);
    dmslz0_(&lon1, &c__0, angs1, ipfile, iflg);
    dmslz0_(&lat1, &c__0, angs1 + 16, ipfile, iflg);
    /* CALL DMSLZ0 (LON2,0,ANGS1(3),IPFILE,IFLG) */
    dmslz0_(&lat2, &c__0, angs1 + 48, ipfile, iflg);
    dmslz0_(&lat0, &c__0, angs1 + 64, ipfile, iflg);
    if (ipfile != NULL) {
        fprintf(ipfile, "\tLongitude of 1st point       = %lf\n", angs1[0]);
        fprintf(ipfile, "\tLatitude of 1st point        = %lf\n", angs1[15]);
        fprintf(ipfile, "\tLongitude of 2nd point       = %lf\n", angs1[31]);
        fprintf(ipfile, "\tLatitude of 2nd point        = %lf\n", angs1[47]);
        fprintf(ipfile, "\tLatitude of origin           = %lf\n", angs1[63]);
    }
    if ((d_1 = lat1 - lat2, fabs(d_1)) <= epsln)
        goto L150;
    con = fabs(lat1);
    if (con <= epsln || (d_1 = con - halfpi, fabs(d_1)) <= epsln)
        goto L150;
    if ((d_1 = fabs(lat0) - halfpi, fabs(d_1)) > epsln)
        goto L160;
  L150:
    if (ipfile != NULL)
        fprintf(ipfile, "pj20z0_0_: Improper parameter\n");
    *iflg = 2002;
    return 0;
  L160:
    singam = sin(gamma);
    cosgam = cos(gamma);
    sinalf = sin(alpha);
    cosalf = cos(alpha);
    if (ipfile != NULL) {
        fprintf(ipfile, "\tFalse easting                = %lf meters\n", x0);
        fprintf(ipfile, "\tFalse northing               = %lf meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf20z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj20z0_0_: Uninitialized transformation\n");
    *iflg = 2000;
    return 0;
  L220:
    sinphi = sin(geog[2]);
    d_1 = geog[1] - lon0;
    dlon = adjlz0_(&d_1);
    vl = sin(bl * dlon);
    if ((d_1 = fabs(geog[2]) - halfpi, fabs(d_1)) > epsln)
        goto L230;
    ul = singam * d_sign(&one, &geog[2]);
    us = al * geog[2] / bl;
    goto L250;
  L230:
    ts = tsfnz0_(&e, &geog[2], &sinphi);
    q = el / pow_dd(&ts, &bl);
    s = 0.5 * (q - 1.0 / q);
    t = 0.5 * (q + one / q);
    ul = (s * singam - vl * cosgam) / t;
    con = cos(bl * dlon);
    if (fabs(con) < tol)
        goto L240;
    us = al * atan((s * cosgam + vl * singam) / con) / bl;
    if (con < 0.0)
        us += pi * al / bl;
    goto L250;
  L240:
    us = al * bl * dlon;
  L250:
    if ((d_1 = fabs(ul) - 1.0, fabs(d_1)) > epsln)
        goto L260;
    if (ipfile != NULL)
        fprintf(ipfile, "pj20z0_0_: Point projects into infinity\n");
    *iflg = 2001;
    return 0;
  L260:
    vs = 0.5 * al * log((1.0 - ul) / (1.0 + ul)) / bl;
    proj[1] = x0 + vs * cosalf + us * sinalf;
    proj[2] = y0 + us * cosalf - vs * sinalf;
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi20z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L280;
    if (ipfile != NULL)
        fprintf(ipfile, "pj20z0_0_: Uninitialized transformation\n");
    *iflg = 2000;
    return 0;
  L280:
    x = proj[1] - x0;
    y = proj[2] - y0;
    vs = x * cosalf - y * sinalf;
    us = y * cosalf + x * sinalf;
    q = exp(-bl * vs / al);
    s = 0.5 * (q - 1.0 / q);
    t = 0.5 * (q + 1.0 / q);
    vl = sin(bl * us / al);
    ul = (vl * cosgam + s * singam) / t;
    if ((d_1 = fabs(ul) - 1.0, fabs(d_1)) >= epsln)
        goto L300;
    geog[1] = lon0;
    geog[2] = d_sign(&halfpi, &ul);
    return 0;
  L300:
    con = 1.0 / bl;
    d_1 = el / sqrt((1.0 + ul) / (1.0 - ul));
    ts = pow_dd(&d_1, &con);
    geog[2] = phi2z0_(&e, &ts, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    con = cos(bl * us / al);
    lon = lon0 - atan2(s * cosgam - vl * singam, con) / bl;
    geog[1] = adjlz0_(&lon);
    return 0;

} /* pj20z0_ */


int pj20z0_()
{
    return pj20z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is20z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj20z0_0_(1, zone, data, ipfile, iflg,(double*)0,(double*)0);
}

int pf20z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj20z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi20z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj20z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj20z0.c ***/
