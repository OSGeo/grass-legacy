/* e2fnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double e2fnz0_(eccnts)
double *eccnts;
{
    double con1 = .05859375, con2 = .75;
    double ret_val;

    /* FUNCTION TO COMPUTE CONSTANT (E2). */
    ret_val = con1 * *eccnts * *eccnts * (1.0 + con2 * *eccnts);
    return ret_val;
} /* e2fnz0_ */
