/* determine latitude angle phi-2 */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_phi2.c,v 4.1 1992/04/03 13:35:38 gie Exp $";
#endif

#include "projects.h"

#define TOL 1.0e-10
#define N_ITER 15

	double
#ifdef __STDC__
pj_phi2(double ts, double e)
#else
pj_phi2(ts, e)
    double ts;
    double e;
#endif
{
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
	return( i ? Phi : HUGE_VAL );
}
