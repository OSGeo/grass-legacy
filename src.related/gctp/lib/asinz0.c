/* asinz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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

double asinz0_(con)
double *con;
{
    double one = 1.0;

    /* FUNCTION ADJUSTS FOR ROUND-OFF ERRORS IN COMPUTING ARCSINE */
    if (fabs(*con) > 1)
        *con = d_sign(&one, con);
    return asin(*con);
} /* asinz0_ */

/*** end asinz0.c ***/
