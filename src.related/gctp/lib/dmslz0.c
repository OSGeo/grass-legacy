/* dmslz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include "gctp.h"

static double c_b4 = 1e-4;
double fabs();

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

int dmslz0_(ang, iunit, dms, ipfile, iflg)
double *ang;
int *iunit;
char *dms;
FILE *ipfile;
int *iflg;
{
    int ideg, isec, imin, iount, sign = 1;
    double sec;

    /* SUBROUTINE TO CONVERT ANGLE TO DISPLAY DMS */

    /* DETERMINE THE SIGN OF ANGLE */
    *iflg = 0;
    if (*ang < 0)
        sign = -1;

    /* CONVERT ANGLE TO SECONDS OF ARC */
    iount = 3;
    sec = fabs(*ang);
    unitz0_(&sec, iunit, &sec, &iount, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    sec -= d_mod(&sec, &c_b4);
    isec = (int)sec;

    /* COMPUTE DEGREES, MINUTES,AND SECONDS PARTS OF ANGLE */
    ideg = isec / 3600;
    isec %= 3600;
    imin = isec / 60;
    isec %= 60;
    /* sec = sec - ideg * 3600 - imin * 60 - isec; */
    sec = sec - ideg * 3600 - imin * 60;

    /* FORM DMS CHARACTER FIELD */
    sprintf(dms, "% 3d:%2d'%6.3f\"", ideg * sign, imin, sec);
    dms[15] = '\0';
    return 0;
} /* dmslz0_ */

/*** end dmslz0.c ***/
