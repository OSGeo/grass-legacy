/* msfnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <math.h>

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double msfnz0_(eccent, sinphi, cosphi)
double *eccent, *sinphi, *cosphi;
{
    double con, ret_val;

    /* FUNCTION TO COMPUTE CONSTANT (SMALL M). */
    con = *eccent * *sinphi;
    ret_val = *cosphi / sqrt(1.0 - con * con);
    return ret_val;
} /* msfnz0_ */

/*** end msfnz0.c ***/
