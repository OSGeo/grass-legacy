#ifndef lint
static char *SCCSID = "@(#)phi1_.c	USGS v.3.1";
#endif
/* determine latitude angle phi-1 */
# include "projects.h"

# define N_ITER 15
# define EPSILON 1.0e-7
# define TOL 1.0e-10

	double
phi1_ (qs) double qs; {
	int i;
	double Phi, sinpi, cospi, con, com, dphi;

	Phi = asin (.5 * qs);
	if (e < EPSILON)
		return( Phi );
	i = N_ITER;
	do {
		sinpi = sin (Phi);
		cospi = cos (Phi);
		con = e * sinpi;
		com = 1. - con * con;
		dphi = .5 * com * com / cospi * (qs / one_es -
		   sinpi / com + .5 / e * log ((1. - con) /
		   (1. + con)));
		Phi += dphi;
	} while (fabs(dphi) > TOL && --i);
	return( i ? Phi : HUGE );
}
