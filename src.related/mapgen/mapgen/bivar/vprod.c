#ifndef lint
static char *SCCSID = "@(#)vprod.c	AMG v.3.1";
#endif
/* vprod - bivariate evaluation
**	'n'th degree univariate evaluation of bivariate polynomial
**	'Cxy'(i,j)*'Tx'(i)*'Ty'(j), i = 0,...,n, j = 0,...,i
*/
	double
vprod(n, Tx, Ty, Cxy) double *Tx, *Ty, *Cxy; {
	register double v, x;
	double *ty;
	register i, j;

	v = 0.;
	for (i = 0; i <= n ; i++) {
		x = *Tx++;
		ty = Ty;
		for (j = 0; j <= i; j++) {
			if (*Cxy) v += x * *ty++ * *Cxy;
			Cxy++;
		}
	}
	return( v );
}
