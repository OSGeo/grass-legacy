static char *SCCSID = "@(#)phi1_.c	AMG v.1.1";
/* determine latitude angle phi-1 */
# include "projects.h"

# define N_ITER 15
# define EPSILON 1.0e-7
# define TOL 1.0e-10

	double
phi1_ (eccent, qs) double eccent, qs; {
	int i;
	double eccnts, phi, sinpi, cospi, con, com, dphi;

	phi = asin (.5 * qs);
	if (eccent < EPSILON)
		return( phi );
	eccnts = eccent * eccent;
	i = N_ITER;
	do {
		sinpi = sin (phi);
		cospi = cos (phi);
		con = eccent * sinpi;
		com = 1. - con * con;
		dphi = .5 * com * com / cospi * (qs / (1. - eccnts) -
		   sinpi / com + .5 / eccent * log ((1. - con) /
		   (1. + con)));
		phi += dphi;
	} while (fabs(dphi) > TOL && --i);
	return( i ? phi : HUGE );
}
