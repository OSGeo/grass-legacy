#ifndef lint
static char *SCCSID = "@(#)cprod.c	AMG v.3.1";
#endif
/* for bivariate product */
	double *
cprod(n, P, Tx, Ty) double *P, *Tx, *Ty; {
	register i, j;

	for (i = 0; i <= n ; i++)
		for (j = 0; j <= i; j++)
			*P++ = Tx[i-j] * Ty[j];

	return P;
}
