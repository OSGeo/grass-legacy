/* e3fnz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/
double e3fnz0_(eccnts)
double *eccnts;
{
    double con = .0113932291666667;
    double ret_val, d_1, d_2;

    /* FUNCTION TO COMPUTE CONSTANT (E3). */

    /* Computing 3rd power */
    d_1 = *eccnts, d_2 = d_1;
    ret_val = con * (d_2 * (d_1 * d_1));

    return ret_val;
} /* e3fnz0_ */

/*** end e3fnz0.c ***/
