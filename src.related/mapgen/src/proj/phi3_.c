static char *SCCSID = "@(#)phi3_.c	AMG v.1.1";
/* determine latitude angle phi-3 */
# include "projects.h"

# define N_ITER 15
# define TOL 1.0e-10

	double
phi3_ (ml, e0) double ml, *e0; {
	double phi, dphi;
	int i;

	phi = ml;
	i = N_ITER;
	do {
		dphi = (ml + e0[1] * sin (2. * phi) - e0[2] * sin (4. * phi)) /
		   e0[0] - phi;
		phi += dphi;
	} while (fabs(dphi) > TOL && --i);
	return ( i ? phi : HUGE);
}
