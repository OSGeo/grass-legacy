/* phi4z0.f -- translated by f2c (version of 11 May 1990  14:38:39).
 * C version and Fortran dependencies removed by Sean Dougherty and
 * Philip Thompson (phils@athena.mit.edu), 10/5/90.
 * Computer Resource Lab., Dept. Architecture and Urban Planning,
 * MIT, Cambridge MA  02139
 */

#include <stdio.h>
#include <math.h>

/**********************************************************************/
/** NOAA/USGS GENERAL MAP PROJECTION PACKAGE ..... DR. A. A. ELASSAL **/
/** GCTP/II                 VERSION 1.0.2           SEPTEMBER 1,1986 **/
/**********************************************************************/

double phi4z0_(eccnts, e0, e1, e2, e3, a, b, c, ipfile, iflg)
double *eccnts, *e0, *e1, *e2, *e3, *a, *b, *c;
FILE *ipfile;
int *iflg;
{
    /* Initialized data */
    double tol = 1e-11;
    int nit = 15;
    /* System generated locals */
    int i_1;
    double ret_val = 0.0;
    /* Local variables */
    double dphi, sin2ph;
    int ii;
    double ml, tanphi, sinphi, phi, mlp, con1, con2, con3;

    /* FUNCTION TO COMPUTE LATITUDE ANGLE (PHI-4). */
    phi = *a;
    i_1 = nit;
    for (ii = 1; ii <= i_1; ++ii) {
        sinphi = sin(phi);
        tanphi = tan(phi);
        *c = tanphi * sqrt(1.0 - *eccnts * sinphi * sinphi);
        sin2ph = sin(2.0 * phi);
        ml = *e0 * phi - *e1 * sin2ph + *e2 * sin(4.0 * phi) - *e3 *
                sin(6.0 * phi);
        mlp = *e0 - 2.0 * *e1 * cos(2.0 * phi) + 4.0 * *e2 * cos(4.0 *
                phi) - 6.0 * *e3 * cos(6.0 * phi);
        con1 = 2.0 * ml + *c * (ml * ml + *b) - 2.0 * *a * (*c * ml + 1.0);
        con2 = *eccnts * sin2ph * (ml * ml + *b - 2.0 * *a * ml) /
                (2.0 * *c);

        con3 = 2.0 * (*a - ml) * (*c * mlp - 2.0 / sin2ph) - 2.0 * mlp;
        dphi = con1 / (con2 + con3);
        phi += dphi;
        if (fabs(dphi) > tol)
            continue;
        ret_val = phi;
        return ret_val;
    }
    if (ipfile != NULL)
        fprintf(ipfile, "phi4z0_: Latitude failed to converge\n");
    *iflg = 24;
    return ret_val;
} /* phi4z0_ */
