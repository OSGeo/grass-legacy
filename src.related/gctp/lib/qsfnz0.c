/* qsfnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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

double qsfnz0_(eccent, sinphi)
double *eccent, *sinphi;
{
    double epsln = 1e-7;
    double ret_val;
    /* Local variables */
    double con;

    /* FUNCTION TO COMPUTE CONSTANT (SMALL Q). */
    if (*eccent < epsln) {
        ret_val = 2.0 * *sinphi;
        return ret_val;
    }
    con = *eccent * *sinphi;
    ret_val = (1.0 - *eccent * *eccent) * (*sinphi / (1.0 - con * con) -
            0.5 / *eccent * log((1.0 - con) / (1.0 + con)));
    return ret_val;
} /* qsfnz0_ */

/*** end qsfnz0.c ***/
