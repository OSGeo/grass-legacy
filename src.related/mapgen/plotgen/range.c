#ifndef lint
static char *SCCSID = "@(#)range.c	AMG v.3.1";
#endif
/* range - scale and plot routines */

# include "plotgen.h"
# include "graphics.h"

extern struct PLTDEF def;
long lrnd();
	void
dpoint (x, y) double x, y; {
	long lx, ly;

	lx = lrnd(x * def.x.scale + def.x.offset);
	if (lx >= def.x.min && lx <= def.x.max) {
		ly = lrnd(y * def.y.scale + def.y.offset);
		if (ly >= def.y.min && ly <= def.y.max)
			pxyxmit(_PENUP, lx, ly);
	}
}

long xlo, xhi, ylo, yhi;
int c0, c1;

static int	pen;	/* current pen status */
static long	xl, yl, xc, yc;

/* flag outside window */
	static
outside (x, y) long x, y; {
	int c;

	c = (x < xlo ? LEFT : (x > xhi ? RIGHT : 0));
	if (y < ylo)
		return (c | BOTTOM);
	if (y > yhi)
		return (c | TOP);
	return (c);
}

/* clip line to range inside of window */
	static
clipper(penup, x0, y0, x1, y1) long x0, y0, x1, y1; {
	register c;
	int cs, cl0, cl1;
	long x, y;

	cs = c0 = cl0 = outside(x0, y0);
	c1 = cl1 = outside(x1, y1);

	while (cl0 | cl1) {
		if (cl0 & cl1)
			return(cl1);
		c = cl0 != 0 ? cl0 : cl1;
		if (c & LEFT) {
			y = y0 + lrnd((y1-y0)*((xlo-x0) /
				((double)(x1-x0))));
			x = xlo;
		}
		else if (c & RIGHT) {
			y = y0 + lrnd((y1-y0)*((xhi-x0) /
				((double)(x1-x0))));
			x = xhi;
		}
		else if (c & BOTTOM) {
			x = x0 + lrnd((x1-x0)*((ylo-y0) /
				((double)(y1-y0))));
			y = ylo;
		}
		else if (c & TOP) {
			x = x0 + lrnd((x1-x0)*((yhi-y0) /
				((double)(y1-y0))));
			y = yhi;
		}
		if (c == cl0) {
			x0 = x;
			y0 = y;
			cl0 = outside(x, y);
		}
		else {
			x1 = x;
			y1 = y;
			cl1 = outside(x, y);
		}
	}

	if (penup | cs)
		draft(1, x0, y0);
	draft(0, x1, y1);

	return 0;
}

	static
scaler(x, y) double x, y; {

	xc = lrnd(x * def.x.scale + def.x.offset);
	yc = lrnd(y * def.y.scale + def.y.offset);
}

/* start new line */
newline(x, y) double x, y; {
	scaler(x, y);
	xl = xc;
	yl = yc;
	pen = 1;
}

/* continue line */
conline(x, y) double x, y; {
	scaler(x, y);
	pen = clipper(pen, xl, yl, xc, yc);
	xl = xc;
	yl = yc;
}
