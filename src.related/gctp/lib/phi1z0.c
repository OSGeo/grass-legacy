/* phi1z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <math.h>
#include <stdio.h>
#include "gctp.h"

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double phi1z0_(eccent, qs, ipfile, iflg)
double *eccent, *qs;
FILE *ipfile;
int *iflg;
{
    double epsln = 1e-7;
    double tol = 1e-10;
    int nit = 15;
    int i_1;
    double ret_val, d_1;
    /* Local variables */
    double dphi, cospi, sinpi;
    int ii;
    double eccnts, com, con, phi;

    /* FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-1). */
    d_1 = 0.5 * *qs;
    ret_val = asinz0_(&d_1);
    if (*eccent < epsln)
        return ret_val;
    eccnts = *eccent * *eccent;
    phi = ret_val;
    i_1 = nit;
    for (ii = 1; ii <= i_1; ++ii) {
        sinpi = sin(phi);
        cospi = cos(phi);
        con = *eccent * sinpi;
        com = 1.0 - con * con;
        dphi = 0.5 * com * com / cospi * (*qs / (1.0 - eccnts) - sinpi
            / com + 0.5 / *eccent * log((1.0 - con) / (1.0 + con)));
        phi += dphi;
        if (fabs(dphi) > tol)
            continue;
        ret_val = phi;
        return ret_val;
    }
    if (ipfile != NULL)
        fprintf(ipfile, "phi1z0_: Latitude failed to converge\n");
    *iflg = 21;
    return ret_val;
} /* phi1z0_ */

/*** end phi1z0.c ***/
