#ifndef lint
static char *SCCSID = "@(#)setscale.c	AMG v.3.1";
#endif
struct SCALE {
	double scale, off;
	double min, max;
};
/* setscale - establish range and scaling factors
**	'a' points to an array of 'n' independent values.
**	'etol' is a factor applied to the range difference to expand
**		the final 'min'-'max' values
**	returns 1 if all values in 'a' equal.
*/
setscale(a, dim, n, etol, scale) double *a, etol; struct SCALE *scale; {
	double del;

		/* determine data range */
	scale->min = scale->max = *a;
	a += dim;
	while (--n > 0) {
		if (*a < scale->min)
			scale->min = *a;
		else if (*a > scale->max)
			scale->max = *a;
		a += dim;
	}

		/* adjust range by 'etol' */
	etol *= scale->max - scale->min;
	scale->min -= etol;
	scale->max += etol;

		/* is final range reasonable? */
	del = scale->max - scale->min;
	if (del == 0.)
		return 1;  /* No! */

		/* determine offset and scale factors */
	del = 1./del;
	scale->scale = 2. * del;
	scale->off = -(scale->min + scale->max) * del;

		/* good scale */
	return 0;
}

/* scale data */
scalit(a, dim, scale, n)
double *a;
struct SCALE *scale;
{

	while (n--) {
		*a = *a * scale->scale + scale->off;
		a += dim;
	}
}
