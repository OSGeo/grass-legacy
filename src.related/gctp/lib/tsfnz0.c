/* tsfnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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

double tsfnz0_(eccent, phi, sinphi)
double *eccent, *phi, *sinphi;
{
    double halfpi = 1.57079632679489661923;
    double ret_val, d_1;
    double com, con;

    /* FUNCTION TO COMPUTE CONSTANT (SMALL T). */

    con = *eccent * *sinphi;
    com = 0.5 * *eccent;
    d_1 = (1.0 - con) / (1.0 + con);
    con = pow_dd(&d_1, &com);
    ret_val = tan(0.5 * (halfpi - *phi)) / con;

    return ret_val;
} /* tsfnz0_ */
