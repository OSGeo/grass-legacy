/* pj18z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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
/* MILLER CYLINDRICAL  */
/**********************************************************************/

int pj18z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    double fortpi = .78539816339744833;
    static int switch_ = 0;
    double d_1;
    char angs[16];
    static double a, x, y, x0, y0;
    static double lon, lon0;

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
        goto L_is18z0;
    case 2:
        goto L_pf18z0;
    case 3:
        goto L_pi18z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is18z0:
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
                "Initialization parameters (miller cylindrical projection)\n");
        fprintf(ipfile, "\tRadius of sphere         = %16.4f meters\n", a);
        fprintf(ipfile, "\tLongitude of c. meridian = %16s\n", angs);
        fprintf(ipfile, "\tFalse easting  = %16.4f meters\n", x0);
        fprintf(ipfile, "\tFalse northing = %16.4f meters\n", y0);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf18z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L120;
    if (ipfile != NULL)
        fprintf(ipfile, "pj18z0_0_: Uninitialized transformation\n");
    *iflg = 1800;
    return 0;
  L120:
    d_1 = geog[1] - lon0;
    lon = adjlz0_(&d_1);
    proj[1] = x0 + a * lon;
    proj[2] = y0 + a * log((tan(fortpi + geog[2] / 2.5))) * 1.25;
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi18z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L220;
    if (ipfile != NULL)
        fprintf(ipfile, "pj18z0_0_: Uninitialized transformation\n");
    *iflg = 1800;
    return 0;
  L220:
    x = proj[1] - x0;
    y = proj[2] - y0;
    d_1 = lon0 + x / a;
    geog[1] = adjlz0_(&d_1);
    geog[2] = 2.5 * atan((exp(y / a / 1.25))) - fortpi * 2.5;
    return 0;

}                               /* pj18z0_ */

int pj18z0_()
{
    return pj18z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is18z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj18z0_0_(1, zone, data, ipfile, iflg,(double*)0,(double*)0);
}

int pf18z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj18z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi18z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj18z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj18z0.c ***/
