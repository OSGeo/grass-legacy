/* untfz0.f -- translated by f2c (version of 11 May 1990  14:38:39). 
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

int untfz0_(inunit, iounit, factor, ipfile, iflg)
int *inunit, *iounit;
double *factor;
FILE *ipfile;
int *iflg;
{
    static int maxunt = 5;
    static double factrs[25] /* was [5][5] */ =
    {
            1.0, 0.0, 0.0, 206264.8062470963, 57.29577951308231, 0.0, 1.0,
            0.3048006096012192, 0.0, 0.0, 0.0, 3.280833333333333, 1.0, 0.0,
            0.0, 4.84813681109536e-6, 0.0, 0.0, 1.0, 2.777777777777778e-4,
            0.0174532925199433, 0.0, 0.0, 3600.0, 1.0
    };

    /* SUBROUTINE TO DETERMINE CONVERGENCE FACTOR BETWEEN TWO LINEAR
     * UNITS */
    /* INPUT ........ */
    /* INUNIT * UNIT CODE OF SOURCE. */
    /* IOUNIT * UNIT CODE OF TARGET. */
    /* IPFILE * LOGICAL NUMBER OF MESSAGES FILE. */

    /* OUTPUT ....... */
    /* FACTOR * CONVERGENCE FACTOR FROM SOURCE TO TARGET. */
    /* IFLG   * RETURN FLAG = 0 , NORMAL RETURN. */
    /* = I , ABNORMAL RETURN. */

    if (*inunit < 0 || *inunit >= maxunt) {
        if (ipfile != NULL)
            fprintf(ipfile,
                    "untfz0_: Illegal source or target unit code = %6d %6d\n",
                    *inunit, *iounit);
        *iflg = 11;
        return 0;
    }
    if (*iounit >= 0 && *iounit < maxunt) {
        *factor = factrs[*iounit + 1 + (*inunit + 1) * 5 - 6];
        if (*factor != 0.0) {
            *iflg = 0;
            return 0;
        }
        if (ipfile != NULL)
            fprintf(ipfile, "untfz0_: Inconsistant unit codes = %6d % 6d\n",
                    *inunit, *iounit);
        *iflg = 12;
    }
    return 0;
} /* untfz0_ */
