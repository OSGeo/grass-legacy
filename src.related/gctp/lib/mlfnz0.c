/* mlfnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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

double mlfnz0_(e0, e1, e2, e3, phi)
double *e0, *e1, *e2, *e3, *phi;
{
    double ret_val;

    /* FUNCTION TO COMPUTE CONSTANT (M). */
    ret_val = *e0 * *phi - *e1 * sin(2.0 * *phi) + *e2 *
            sin(4.0 * *phi) - *e3 * sin(6.0 * *phi);
    return ret_val;
} /* mlfnz0_ */
