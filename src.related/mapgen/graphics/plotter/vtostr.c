#ifndef lint
static char *SCCSID = "@(#)vtostr.c	USGS v.4.1";
#endif
/* basic formating to and from meta-graphics */
	/* convert 'long' value into 3 byte string */
vtostr(v, str) long v; char *str; {
	static long nmask[] = {~0x7f, ~0x7fff, ~0x7fffff},
		    pmask[] = { 0x7f,  0x7fff,  0x7fffff};
	register m;
	int n;

	m = 0;
	if (v < 0) while(m <= 2 && v < nmask[m++]) ; 
	else if (v > 0) while(m <= 2 && v > pmask[m++]) ;

	n = m;
	while (--m > 0) { /* in reverse order */
		*str++ = v & 0xff;
		v >>= 8;
	}
	*str = v & 0xff;
	return (n);
}
	/* Convert double value input */
cvtoxy(mode, plotin, xv, yv) int mode, (*plotin); long *xv, *yv; {
	long strtov();

	*xv = strtov(mode >> 2, plotin);
	*yv = strtov(mode, plotin);
}
	/* convert 'n' bytes into 'long' (inverse of 'vtostr') */
	long
strtov(n, plotin) int n, (*plotin)(); {
	long v;

	if ((n &= 3) > 0) {
		v = (*plotin)();
		if (v > 127) v |= ~0xff;
		while (--n) v = (v << 8) + (*plotin)();
		return(v);
	}
	return(0);
}
