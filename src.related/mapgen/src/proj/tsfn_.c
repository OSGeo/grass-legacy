static char *SCCSID = "@(#)tsfn_.c	AMG v.1.1";
/* determine small t */
# include "projects.h"

	double
tsfn_ (eccent,phi,sinphi) double eccent, phi, sinphi; {
	sinphi *= eccent;
	return (tan (.5 * (HALFPI - phi)) /
	   pow((1. - sinphi) / (1. + sinphi), .5 * eccent));
}
