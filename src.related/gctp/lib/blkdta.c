/* blkdta000.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */
/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

#include <stdio.h>
#include "gctp.h"

/* INITIALIZATION OF ELLIPSOID TO CLARK'S 1866 PARAMETERS. */

/* Global Declarations */

struct _ellpz0_1 ellpz0 = {
    6378206.4, 0.08227185422300323, 0.006768657997291094,
    0.9983056818784341, 0.002542555507651308, 2.698084527466011e-6,
    3.53308873963553e-9, 1.003393903560134
};

struct _sphrz0_1 sphrz0 = {
    6370997.0
};

/*** end blkdta.c ***/
