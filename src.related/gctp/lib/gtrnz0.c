/* gtrnz0.f -- translated by f2c (version of 11 May 1990  14:38:39). */
/* C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include "gctp.h"

/* Table of constant values */

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

int gtrnz0_(crdin, indef, tparin, crdio, iodef, tpario, ipfile, iflg)
double *crdin;
int *indef;
double *tparin, *crdio;
int *iodef;
double *tpario;
FILE *ipfile;
int *iflg;
{
    static int sysunt[21] =
        {0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2};
    int maxunt = 5;
    int maxsys = 20;
    double zero = 0.0;
    double coord[2];
    int i, iunit;

    /* GENERAL PROGRAM FOR TRANSFORMATION BETWEEN VARIOUS REFERENCE
     * SYSTEMS */

    /* INPUT **********************************************************/
    /* CRDIN    : COORDINATES IN INPUT SYSTEM (2 DP WORDS ARRAY). */
    /* INDEF(1) : CODE NUMBER OF INPUT COORDINATE SYSTEM (INTEGER). */
    /* =  0 , GEOGRAPHIC */
    /* =  1 , U T M */
    /* =  2 , STATE PLANE */
    /* =  3 , ALBERS CONICAL EQUAL-AREA */
    /* =  4 , LAMBERT CONFORMAL CONIC */
    /* =  5 , MERCATOR */
    /* =  6 , POLAR STEREOGRAPHIC */
    /* =  7 , POLYCONIC */
    /* =  8 , EQUIDISTANT CONIC */
    /* =  9 , TRANSVERSE MERCATOR */
    /* = 10 , STEREOGRAPHIC */
    /* = 11 , LAMBERT AZIMUTHAL EQUAL-AREA */
    /* = 12 , AZIMUTHAL EQUIDISTANT */
    /* = 13 , GNOMONIC */
    /* = 14 , ORTHOGRAPHIC */
    /* = 15 , GENERAL VERTICAL NEAR-SIDE PERSPECTIVE */
    /* = 16 , SINUSOIDAL */
    /* = 17 , EQUIRECTANGULAR */
    /* = 18 , MILLER CYLINDRICAL */
    /* = 19 , VAN DER GRINTEN I */
    /* = 20 , OBLIQUE MERCATOR (HOTINE) */
    /* INDEF(2) : CODE NUMBER OF INPUT COORDINATE ZONE (INTEGER). */
    /* TPARIN   : PARAMETERS OF INPUT REFERENCE SYSTEM (15 DP WORDS
     * ARRAY). */

    /* INDEF(3) : CODE NUMBER OF UNITS OF MEASURE FOR INPUT COORDS
     * (INTEGER). */
    /* = 0 , RADIANS. */
    /* = 1 , FEET. */
    /* = 2 , METERS. */
    /* = 3 , SECONDS OF ARC. */
    /* = 4 , DEGREES OF ARC. */
    /* = 5 , PACKED DMS. */
    /* IODEF(1) : CODE NUMBER OF OUTPUT COORDINATE SYSTEM (INTEGER). */
    /* IODEF(2) : CODE NUMBER OF OUTPUT COORDINATE ZONE (INTEGER). */
    /* TPARIO   : PARAMETERS OF OUTPUT REFERENCE SYSTEM (15 DP WORDS
     * ARRAY). */
    /* IODEF(3) : CODE NUMBER OF UNITS OF MEASURE FOR OUTPUT COORDS
     * (INTEGER) */
    /* IPFILE   : LOGICAL NUMBER OF FILE FOR MESSAGES. */

    /* OUTPUT *********************************************************/
    /* CRDIO    : COORDINATES IN OUTPUT REFERENCE SYSTEM (2 DP WORDS
     * ARRAY). */
    /* IFLG     : RETURN FLAG (INTEGER). */
    /* = 0 , SUCCESSFUL TRANSFORMATION. */
    /* = I , UNSUCCESSFUL TRANSFORMATION. ERROR CODE   = I. */

    /* Parameter adjustments */
    --tpario;
    --iodef;
    --crdio;
    --tparin;
    --indef;
    --crdin;

    /* Function Body */

    /* CHECK VALIDITY OF CODES FOR UNITS OF MEASURE AND REFERENCE
     * SYSTEMS. */

    if (indef[1] >= 0 && indef[1] <= maxsys) {
        if (iodef[1] >= 0 && iodef[1] <= maxsys) {
            if (indef[3] >= 0 && indef[3] <= maxunt) {
                if (iodef[3] >= 0 && iodef[3] <= maxunt) {
                    goto L80;
                }
                if (ipfile != NULL)
                    fprintf(ipfile, "gtrnz0_: Illegal target unit code = %d\n",
                            iodef[3]);
                *iflg = 4;
                return 0;
            }
            if (ipfile != NULL)
                fprintf(ipfile, 
                        "gtrnz0_: illegal source unit code = %d\n", 
            indef[3]);
            *iflg = 3;
            return 0;
        }
        if (ipfile != NULL)
            fprintf(ipfile, 
                 "gtrnz0_: Illegal target reference system code = %d\n",
                    iodef[1]);
        *iflg = 2;
        return 0;
    }
    if (ipfile != NULL)
        fprintf(ipfile, 
                "gtrnz0_: Illegal source reference system code = %d\n",
                indef[1]);
    *iflg = 1;
    return 0;

  L80:                          /* CHECK CONSISTENCY BETWEEN UNITS OF
                                 * MEASURE AND REFERENCE SYSTEM. */
    iunit = sysunt[indef[1]];
    for (i = 1; i <= 2; ++i) {
        unitz0_(&crdin[i], &indef[3], &coord[i - 1], 
                &iunit, ipfile, iflg);
        if (*iflg != 0)
            return 0;
    }
    iunit = sysunt[iodef[1]];
    unitz0_(&zero, &iunit, &crdio[1], &iodef[3], ipfile, iflg);
    if (*iflg != 0) {
        return 0;
    }
    if (indef[1] != iodef[1] || indef[2] != iodef[2]) {
        /* COMPUTE TRANSFORMED COORDINATES AND ADJUST THEIR UNITS. */
        if (indef[1] == 0)
            goto L520;
        if (indef[2] > 60 || indef[1] == 1)
            goto L200;
        if (ipfile != NULL)
            fprintf(ipfile, "gtrnz0_: Illegal source zone number = %d\n", 
            indef[2]);
        *iflg = 5;
        return 0;
    }
    for (i = 1; i <= 2; ++i) {
        unitz0_(&crdin[i], &indef[3], &crdio[i], &iodef[3], ipfile, iflg);
        if (*iflg != 0)
            return 0;
    }
    return 0;

  L200:                 /* INVERSE TRANSFORMATION. */
    switch ((int) (indef[1])) {
    case 1:
        is01z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi01z0_(coord, &crdio[1], iflg);
        break;
    case 2:
        is02z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi02z0_(coord, &crdio[1], iflg);
        break;
    case 3:
        is03z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi03z0_(coord, &crdio[1], iflg);
        break;
    case 4:
        is04z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi04z0_(coord, &crdio[1], iflg);
        break;
    case 5:
        is05z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi05z0_(coord, &crdio[1], iflg);
        break;
    case 6:
        is06z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi06z0_(coord, &crdio[1], iflg);
        break;
    case 7:
        is07z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi07z0_(coord, &crdio[1], iflg);
        break;
    case 8:
        is08z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi08z0_(coord, &crdio[1], iflg);
        break;
    case 9:
        is09z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi09z0_(coord, &crdio[1], iflg);
        break;
    case 10:
        is10z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi10z0_(coord, &crdio[1], iflg);
        break;
    case 11:
        is11z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi11z0_(coord, &crdio[1], iflg);
        break;
    case 12:
        is12z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi12z0_(coord, &crdio[1], iflg);
        break;
    case 13:
        is13z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi13z0_(coord, &crdio[1], iflg);
        break;
    case 14:
        is14z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi14z0_(coord, &crdio[1], iflg);
        break;
    case 15:
        is15z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi15z0_(coord, &crdio[1], iflg);
        break;
    case 16:
        is16z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi16z0_(coord, &crdio[1], iflg);
        break;
    case 17:
        is17z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi17z0_(coord, &crdio[1], iflg);
        break;
    case 18:
        is18z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi18z0_(coord, &crdio[1], iflg);
        break;
    case 19:
        is19z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi19z0_(coord, &crdio[1], iflg);
        break;
    case 20:
        is20z0_(&indef[2], &tparin[1], ipfile, iflg);
        if (*iflg == 0)
            pi20z0_(coord, &crdio[1], iflg);
        break;
    }
    if (*iflg != 0)
        return 0;
    if (iodef[1] == 0)
        goto L920;
    coord[0] = crdio[1];
    coord[1] = crdio[2];
  L520:
    if (iodef[2] > 60 || iodef[1] == 1)
        goto L540;
    if (ipfile != NULL)
        fprintf(ipfile, 
                "gtrnz0_: Illegal target zone number = %d\n", iodef[2]);
    *iflg = 6;
    return 0;

  L540:                 /* FORWARD TRANSFORMATION. */
    switch (iodef[1]) {
    case 1:
        is01z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf01z0_(coord, &crdio[1], iflg);
        break;
    case 2:
        is02z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf02z0_(coord, &crdio[1], iflg);
        break;
    case 3:
        is03z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf03z0_(coord, &crdio[1], iflg);
        break;
    case 4:
        is04z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf04z0_(coord, &crdio[1], iflg);
        break;
    case 5:
        is05z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf05z0_(coord, &crdio[1], iflg);
        break;
    case 6:
        is06z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf06z0_(coord, &crdio[1], iflg);
        break;
    case 7:
        is07z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf07z0_(coord, &crdio[1], iflg);
        break;
    case 8:
        is08z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf08z0_(coord, &crdio[1], iflg);
        break;
    case 9:
        is09z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf09z0_(coord, &crdio[1], iflg);
        break;
    case 10:
        is10z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf10z0_(coord, &crdio[1], iflg);
        break;
    case 11:
        is11z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf11z0_(coord, &crdio[1], iflg);
        break;
    case 12:
        is12z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf12z0_(coord, &crdio[1], iflg);
        break;
    case 13:
        is13z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf13z0_(coord, &crdio[1], iflg);
        break;
    case 14:
        is14z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf14z0_(coord, &crdio[1], iflg);
        break;
    case 15:
        is15z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf15z0_(coord, &crdio[1], iflg);
        break;
    case 16:
        is16z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf16z0_(coord, &crdio[1], iflg);
        break;
    case 17:
        is17z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf17z0_(coord, &crdio[1], iflg);
        break;
    case 18:
        is18z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf18z0_(coord, &crdio[1], iflg);
        break;
    case 19:
        is19z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf19z0_(coord, &crdio[1], iflg);
        break;
    case 20:
        is20z0_(&iodef[2], &tpario[1], ipfile, iflg);
        if (*iflg == 0)
            pf20z0_(coord, &crdio[1], iflg);
        break;
    }
    if (*iflg != 0)
        return 0;
  L920:
    for (i = 1; i <= 2; ++i) {
        unitz0_(&crdio[i], &iunit, &crdio[i], &iodef[3], ipfile, iflg);
        if (*iflg != 0)
            return 0;
    }
    return 0;
}

/*** end gtrnz0.c ***/
