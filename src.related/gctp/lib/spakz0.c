/* spakz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
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

double spakz0_(ang, ipfile, iflg)
double *ang;
FILE *ipfile;
int *iflg;
{
    int i;
    double factor, deg, sec, min_;
    double ret_val = 0.0, fabs();

    /* FUNCTION TO CONVERT SECONDS OF ARC TO PACKED DMS */

    /* SEPERATE DEGREE FIELD. */
    *iflg = 0;
    factor = 1.0;
    if (*ang < 0.0)
        factor = -1.0;

    sec = fabs(*ang);
    i = (int) (sec / 3600);
    if (i > 360) {      /* ERROR DETECTED IN DMS FORM. */
        if (ipfile != NULL)
            fprintf(ipfile, "spakz0_: Angle greater than 360 degrees = %15.3f\n",
                    *ang);
        *iflg = 13;
        return ret_val;
    }
    deg = (double) i;

    /* SEPERATE MINUTES FIELD. */
    sec -= deg * 3600;
    i = (int) (sec / 60.0);
    min_ = (double) i;

    /* SEPERATE SECONDS FIELD. */
    sec -= min_ * 60.0;
    sec = factor * (deg * 1.0e4 + min_ * 100.0 + sec);
    ret_val = sec;
    return ret_val;
} /* spakz0_ */
