/* phi3z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <math.h>
#include <stdio.h>

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double phi3z0_(ml, e0, e1, e2, e3, ipfile, iflg)
double *ml, *e0, *e1, *e2, *e3;
FILE *ipfile;
int *iflg;
{
    double tol = 1e-11;
    int nit = 15;
    int i_1;
    double ret_val = 0.0;
    int ii;
    double dphi, phi;

    /* FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-3). */
    phi = *ml;
    i_1 = nit;
    for (ii = 1; ii <= i_1; ++ii) {
        dphi = (*ml + *e1 * sin(2.0 * phi) - *e2 * sin(4.0 * phi) +
                *e3 * sin(6.0 * phi)) / *e0 - phi;
        phi += dphi;
        if (fabs(dphi) > tol)
            continue;
        ret_val = phi;
        return ret_val;
    }
    if (ipfile != NULL)
        fprintf(ipfile, "phi3z0_: Latitude failed to converge\n");
    *iflg = 23;
    return ret_val;
} /* phi3z0_ */

/*** phi3z0.c ***/
