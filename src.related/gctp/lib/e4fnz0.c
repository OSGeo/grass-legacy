/* e4fnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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
double e4fnz0_(eccent)
double *eccent;
{
    double com, con;
    double ret_val;

    /* FUNCTION TO COMPUTE CONSTANT (E4). */
    con = 1.0 + *eccent;
    com = 1.0 - *eccent;
    ret_val = sqrt(pow_dd(&con, &con) * pow_dd(&com, &com));
    return ret_val;
}                               /* e4fnz0_ */

/*** end e4fnz0.c ***/
