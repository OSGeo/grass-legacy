#ifndef lint
static char *SCCSID = "@(#)phi2_.c	USGS v.3.1";
#endif
/* determine latitude angle phi-2 */
# include "projects.h"

# define TOL 1.0e-10
# define N_ITER 15

	double
phi2_ (ts) double ts; {
	double eccnth, Phi, con, dphi;
	int i;

	eccnth = .5 * e;
	Phi = HALFPI - 2. * atan (ts);
	i = N_ITER;
	do {
		con = e * sin (Phi);
		dphi = HALFPI - 2. * atan (ts * pow((1. - con) /
		   (1. + con), eccnth)) - Phi;
		Phi += dphi;
	} while ( fabs(dphi) > TOL && --i);
	return( i ? Phi : HUGE );
}
