#ifndef lint
static char *SCCSID = "@(#)mnp.c	AMG v.3.1";
#endif
/* mnp - monomial polynomial coefficients
**	't' - vector to be filled with 'x'^i, i=0,...,'n'
*/
mnp(t, x, n) double t[]; register double x; {
	register i;

	t[0] = 1.;
	for ( i = 1 ; i <= n ; ++i )
		t[i] = x * t[i - 1];
}
