static char *SCCSID = "@(#)qsfn_.c	AMG v.1.1";
/* determine small q */
# include <math.h>

# define EPSILON 1.0e-7

	double
qsfn_ (eccent,sinphi) double eccent, sinphi; {
	double con;

	if (eccent >= EPSILON) {
		con = eccent * sinphi;
		return ((1. - eccent * eccent) * (sinphi / (1. - con * con) -
		   (.5 / eccent) * log ((1. - con) / (1. + con))));
	} else
		return (sinphi + sinphi);
}
