#ifndef lint
static char *SCCSID = "@(#)lrnd.c	OEMG v.1.1";
#endif
/*
** synopsis: perform 'rounding' of double arg to 'long'
*/
	long
lrnd(x) double x; {
	return (x < 0. ? x - .5 : x + .5);
}
