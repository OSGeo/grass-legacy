#ifndef lint
static char *SCCSID = "@(#)apll.c	AMG v.3.1";
#endif
/* apll - create normal equations */
apll(ffct, n, ip, p, work) double (*ffct)(); double p[], work[]; {
	double aux, wgt, *w;
	int i, k, l;

		/* check for formal errors in specified dimensions */
	if (n <= 0 || ip <= 0 || ip > n)
		return(1);

		/* set working storage and right hand side to zero */
	w = work;
	i = ((ip+1) * (ip+2)) / 2;
	while ( --i )
		*w++ = 0.;

		/* start great loop over all given points */
	for (i = 1; i <= n; i++) {
		wgt = (*ffct)(i, p);
		w = work;
		for (k = 0; k <= ip; k++ ) {
			aux = p[k] * wgt;
			for (l = 0; l <= k; l++)
				{ *w += p[l] * aux; ++w; }
/*
				*w++ += p[l] * aux;
*/
		}
	}
	return(0);
}
