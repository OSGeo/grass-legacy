/* pj01z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include <math.h>
#include "gctp.h"

static int c__5 = 5, c__4 = 4;

/**********************************************************************/
/*** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL */
/*** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 */
/**********************************************************************/
/**  U T M  **/
/**********************************************************************/

/* assumes geog input is in latitude & longitude in radians
 * proj output is in easting & northing in meters */
int pj01z0_0_(n__, zone, data, ipfile, iflg, geog, proj)
int n__;
int *zone;
double *data;
FILE *ipfile;
int *iflg;
double *geog, *proj;
{
    static int switch_ = 0;
    int ind;
    double buffl[8], con, lon0;

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
        goto L_is01z0;
    case 2:
        goto L_pf01z0;
    case 3:
        goto L_pi01z0;
    }

    /* .  INITIALIZATION OF PROJECTION PARAMETERS (ENTRY INPUT)  . */
  L_is01z0:
    *iflg = 0;
    if (switch_ != 0 && switch_ == *zone)
        return 0;
    if ((double)(*zone) == 0.0)
        goto L40;
    if (*zone >= 1 && *zone <= 60)
        goto L100;
    if (ipfile != NULL)
        fprintf(ipfile, "pj01z0_0_: Illegal zone no. = %6d\n", *zone);
    *iflg = 101;
    return 0;

  L40:
    unitz0_(&data[3], &c__5, &con, &c__4, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    *zone = (int) (con / 6.);
    ind = 1;
    if (*zone < 0)
        ind = 0;
    *zone = (*zone + 30) % 60 + ind;
    if (*zone < 0) {
        goto L60;
    } else if (*zone == 0) {
        goto L80;
    } else {
        goto L100;
    }
  L60:
    *zone += 60;
    goto L100;
  L80:
    *zone = 1;
  L100:
    buffl[0] = data[1];
    buffl[1] = data[2];
    buffl[2] = .9996;
    buffl[3] = 0.0;
    lon0 = (double)(*zone * 6 - 183);
    unitz0_(&lon0, &c__4, &buffl[4], &c__5, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    buffl[5] = 0.0;
    buffl[6] = 5e5;
    buffl[7] = 0.0;
    if (data[4] < 0.0)
        buffl[7] = 1e7;
    switch_ = 0;
    is09z0_(zone, buffl, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    /* LIST RESULTS OF PARAMETER INITIALIZATION. */

    if (ipfile != NULL) {
        fprintf(ipfile, 
                "Initialization parameters (u t m projection):\n");
        fprintf(ipfile, "\tZone = %2d\n", *zone);
    }
    switch_ = *zone;
    return 0;

    /* .  FORWARD TRANSFORMATION  . */
  L_pf01z0:
    *iflg = 0;
    if (switch_ != 0) {
        pf09z0_(&geog[1], &proj[1], iflg);
        return 0;
    }
    if (ipfile != NULL)
        fprintf(ipfile, "pj01z0_0_: Uninitialized transformation\n");
    *iflg = 100;
    return 0;

    /* .  INVERSE TRANSFORMATION  . */
  L_pi01z0:
    *iflg = 0;
    if (switch_ != 0)
        goto L160;
    if (ipfile != NULL)
        fprintf(ipfile, "pj01z0_0_: Uninitialized transformation\n");
    *iflg = 100;
    return 0;

  L160:
    pi09z0_(&proj[1], &geog[1], iflg);
    return 0;

}                               /* pj01z0_ */

int pj01z0_()
{
    return pj01z0_0_(0, (int*)0, (double*)0, NULL, (int*)0,
            (double*)0, (double*)0);
}

int is01z0_(zone, data, ipfile, iflg)
int *zone;
double *data;
FILE *ipfile;
int *iflg;
{
    return pj01z0_0_(1, zone, data, ipfile, iflg,(double*)0,(double*)0);
}

int pf01z0_(geog, proj, iflg)
double *geog, *proj;
int *iflg;
{
    return pj01z0_0_(2, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

int pi01z0_(proj, geog, iflg)
double *proj, *geog;
int *iflg;
{
    return pj01z0_0_(3, (int*)0, (double*)0, NULL, iflg, geog, proj);
}

/*** end pj01z0.c ***/
