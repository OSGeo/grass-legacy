/* unitz0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include "gctp.h"

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

int unitz0_(parin, inunit, pario, iounit, ipfile, iflg)
double *parin;
int *inunit;
double *pario;
int *iounit;
FILE *ipfile;
int *iflg;
{
    int inunt, iount;
    double factor, cin, cio;

    /* SUBROUTINE TO TRANSFORM UNITS OF MEASURE FOR A PARAMETER */

    /* INPUT ...... */
    /* INUNIT * UNIT CODE OF SOURCE */
    /* PARIN  * SOURCE PARAMETER */
    /* IOUNIT * UNIT CODE OF TARGET */
    /* IPFILE * LOGICAL NUMBER OF MESSAGES FILE. */

    /* OUTPUT ..... */
    /* PARIO  * TARGET PARAMETER */
    /* IFLG   * RETURN FLAG */

    *iflg = 0;
    if (*inunit == *iounit) {
        *pario = *parin;
        return 0;
    }

    /* ADJUST FOR PACKED DMS UNITS */
    if (*inunit == 5) {
        inunt = 3;
        cin = paksz0_(parin, ipfile, iflg);
        if (*iflg != 0)
            return 0;
    } else {
        inunt = *inunit;
        cin = *parin;
    }
    if (*iounit == 5)
        iount = 3;
    else
        iount = *iounit;

    /* COMPUTE TRANSFORMATION FACTOR */

    untfz0_(&inunt, &iount, &factor, ipfile, iflg);
    if (*iflg != 0)
        return 0;
    cio = factor * cin;

    /* ADJUST OUTPUT FOR DMS UNITS */

    if (*iounit == 5) {
        *pario = spakz0_(&cio, ipfile, iflg);
        if (*iflg != 0)
            return 0;
    } else
        *pario = cio;
    return 0;
} /* unitz0_ */
