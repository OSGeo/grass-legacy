/* determine small q */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_qsfn.c,v 4.1 1992/04/03 13:35:38 gie Exp $";
#endif
#include <math.h>
# define EPSILON 1.0e-7
	double
#ifdef __STDC__
pj_qsfn(double sinphi, double e, double one_es)
#else
pj_qsfn(sinphi, e, one_es)
    double sinphi, e, one_es;
#endif
{
	double con;

	if (e >= EPSILON) {
		con = e * sinphi;
		return (one_es * (sinphi / (1. - con * con) -
		   (.5 / e) * log ((1. - con) / (1. + con))));
	} else
		return (sinphi + sinphi);
}
