#ifndef lint
static char *SCCSID = "@(#)lrnd.c	USGS v.4.1";
#endif
/* synopsis: perform 'rounding' of double arg to 'long' */
	long
lrnd(x) double x; {
	return (x < 0. ? x - .5 : x + .5);
}
