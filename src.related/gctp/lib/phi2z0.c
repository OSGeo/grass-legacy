/* phi2z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include <math.h>
#include "gctp.h"

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double phi2z0_(eccent, ts, ipfile, iflg)
double *eccent, *ts;
FILE *ipfile;
int *iflg;
{
    int ii, i_1, nit = 15;
    double tol = 1e-10, halfpi = 1.57079632679489661923;
    double ret_val = 0.0, d_1;
    double dphi, sinpi, eccnth, con, phi;

    /* FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-2). */
    eccnth = 0.5 * *eccent;
    phi = halfpi - 2.0 * atan(*ts);
    i_1 = nit;
    for (ii = 1; ii <= i_1; ++ii) {
        sinpi = sin(phi);
        con = *eccent * sinpi;
        d_1 = (1.0 - con) / (1.0 + con);
        dphi = halfpi - 2.0 * atan(*ts * pow_dd(&d_1, &eccnth)) - phi;
        phi += dphi;
        if (fabs(dphi) > tol)
            continue;
        ret_val = phi;
        return ret_val;
    }
    if (ipfile != NULL)
        fprintf(ipfile, "phi2z0_: Latitude failed to converge\n");
    *iflg = 22;
    return ret_val;
} /* phi2z0_ */

/*** end phi2z0.c ***/
