/* e0fnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double e0fnz0_(eccnts)
double *eccnts;
{
    double ret_val;             /* System generated locals */

    /* FUNCTION TO COMPUTE CONSTANT (E0). */
    ret_val = 1.0 - 0.25 * *eccnts * (1.0 + *eccnts / 16.0 *
            (3.0 + 1.25 * *eccnts));
    return ret_val;
} /* e0fnz0_ */

/*** end e0fnz0.c ***/
