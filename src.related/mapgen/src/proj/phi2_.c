static char *SCCSID = "@(#)phi2_.c	AMG v.1.1";
/* determine latitude angle phi-2 */
# include "projects.h"

# define TOL 1.0e-10
# define N_ITER 15

	double
phi2_ (eccent, ts) double eccent, ts; {
	double eccnth, phi, con, dphi;
	int i;

	eccnth = .5 * eccent;
	phi = HALFPI - 2. * atan (ts);
	i = N_ITER;
	do {
		con = eccent * sin (phi);
		dphi = HALFPI - 2. * atan (ts * pow((1. - con) /
		   (1. + con), eccnth)) - phi;
		phi += dphi;
	} while ( fabs(dphi) > TOL && --i);
	return( i ? phi : HUGE );
}
