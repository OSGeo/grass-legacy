#ifndef lint
static char *SCCSID = "@(#)ffct.c	AMG v.3.1";
#endif
/* ffct - generate bfactsiate observational vector for 'apll'
**	'i'th observation (1 to --)
**	'p' loaded with fundamental equation values followed by
**		functional value.
**	returns weight associated with value.
**
**	structure 'bfacts' must, of course, be already set up.
*/
#include "bivar.h"
	double
ffct(i, p) double *p; {
	double *cprod(), *q;

	q = bfacts.xyzw + (i-1) * bfacts.wghts;
		/* form univariate vectors */
	(*bfacts.np)(bfacts.Tx, *q++, bfacts.deg);
	(*bfacts.np)(bfacts.Ty, *q++, bfacts.deg);

		/* form bivariate vector */
	*cprod(bfacts.deg, p, bfacts.Tx, bfacts.Ty) = *q;

		/* return weight */
	if (bfacts.wghts > 3)
		return( *++q );
	else
		return( 1. );
}
