#ifndef lint
static char *SCCSID = "@(#)plroff.c	AMG v.3.2";
#endif
/* troff to 'plotter' control */
# include <stdio.h>
# include <graphics.h>

# define FONTSIZE 32
# define CM_IN 2.54
# define POINT 72
# define CONV 508.

BASE base;
	static double
ffact,		/* font scaling factor */
xyfact;		/* position scaling factor */
	static long
mpen = 0,	/* selected mechanical pen */
size,		/* selected size */
xoff = 0,	/* page offset */
p_width = 0;	/* horizontal offset for page width */
	static short
npens = 0,
penlist[20];
	static char
penname[] = "ATR";

	void
plotout(c) { putchar(c); }
	void
init(width, length) {
	extern largc;
	extern char **largv;

	if (largc > 1)
		mpen = atoi(largv[1]);
	plotopt(NEWPEN, penname+1);
	plotopt(MPEN, (long)mpen);
	p_width = width;
}
	void
setres(res, h, v) {
	xyfact = CONV / res;
	ffact = - (CONV * 16) / (POINT * FONTSIZE);
}
	void
loadfont(n, s) { /* initialize defer file and pens */
	if (!xoff) {
		penlist[npens++] = n;
		*penname = n + 'A';
		plotopt(NEWPEN, penname);
		plotopt(MPEN, (long)mpen);
		plotopt(LINKXY, penname+1);
		plotopt(SFONTS, s);
	}
}
	void
stop() { /* cleanup and remove pens */
	while (npens) {
		*penname = npens + 'A';
		plotopt(SPEN, penname);
		plotopt(DELPEN);
		--npens;
	}
	plotopt(SPEN, penname+1);
	plotopt(DELPEN);
}
	void
setfont(n) {
	*penname = 'A' + n;
	plotopt(SPEN, penname);
	plotopt(SSIZE, size);
}
	void
setsize(n) int n; {
	size = n * ffact - .5;
	plotopt(SSIZE, size);
}
	void
print(x, y, c) {
	plotopt(SYM, (long)c);
	moveto((long)(xyfact * (xoff + x) + .5), (long)(- xyfact * y - .5));
}
	void
newpage(n) {
	xoff = (n - 1) * p_width;
}
	void /* alternate paper width */
useropt(s) char *s; {
	char c = 0;
	float f;

	if (sscanf(s,"%f%c", &f, &c)) {
		switch (c) {
		case 'c': /* in centimeters */
			f *= 200;
			break;
		case 'p': /* in points */
			f /= 72; 
		case 'i': /* in inches */
		case 0:
			f *= 200. * CM_IN;
			break;
		default:
			fprintf(stderr,"invalid width qualifier");
			exit(1);
		}
		p_width = f / xyfact + .5;
	}
}
	void
userline(str) char *str; {}
	void
sprint() {}
