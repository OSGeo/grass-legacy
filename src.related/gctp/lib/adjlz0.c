/* adjlz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */
/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double adjlz0_(lon)
double *lon;
{
    /* Initialized data */
    double pi = 3.14159265358979323846;
    double twopi, fabs();

    /* FUNCTION TO ADJUST LONGITUDE ANGLE TO MODULE 180 DEGREES. */

    twopi = (*lon / (*lon)) * 2 * pi;   /* watch out for "comment" */
    while (fabs(*lon) > pi)
        *lon -= twopi;
    return *lon;
}                               /* adjlz0_ */
