/* paksz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double paksz0_(ang, ipfile, iflg)
double *ang;
FILE *ipfile;
int *iflg;
{
    int i;
    double c1 = 3600.0, c2 = 60.0, fabs();
    double factor, deg, sec, min_, tmp;
    double ret_val = 0.0;

    /* FUNCTION TO CONVERT DMS PACKED ANGLE INTO SECONDS OF ARC. */

    /* SEPERATE DEGREE FIELD. */
    *iflg = 0;
    factor = 1.0;
    if (*ang < 0.0)
        factor = -1.0;
    sec = fabs(*ang);
    tmp = 1.0e4;
    i = (int)(sec / tmp);
    if (i > 360)
        goto L20;
    deg = (double)i;

    /* SEPERATE MINUTES FIELD. */
    sec -= deg * tmp;
    tmp = 100.0;
    i = (int) (sec / tmp);
    if (i > 60)
        goto L20;
    min_ = (double) i;

    /* SEPERATE SECONDS FIELD. */
    sec -= min_ * tmp;
    if (sec > c2)
        goto L20;
    sec = factor * (deg * c1 + min_ * c2 + sec);
    return sec;                 /* done */

  L20:
    /* ERROR DETECTED IN DMS FORM. */
    if (ipfile != NULL)
        fprintf(ipfile, "paksz0_: Illegal packed dms field = %15.3f\n", *ang);
    *iflg = 10;
    return ret_val;
} /* paksz0_ */

/*** end paksz0.c ***/
