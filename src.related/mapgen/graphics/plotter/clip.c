#ifndef lint
static char *SCCSID = "@(#)clip.c	USGS v.4.1";
#endif
/* provides basic windowing of vectors.  */
# define PLOTTER
# include "graphics.h"
# include "plotter.h"

# define caseof(x) case x & _LBMASK
# define LEFT	0x8
# define RIGHT  0x4
# define BOTTOM	0x2
# define TOP	0x1
	extern int
(*device)();
	extern long
lrnd();
	extern PEN *
pen;
	extern long
base_x, base_y;

# define XLO pen->xy->xlo
# define XHI pen->xy->xhi
# define YLO pen->xy->ylo
# define YHI pen->xy->yhi
	static long
XMAX, YMAX;
	/* set maximum range (initialization) */
windinit(x, y) long x, y; {
	XMAX = x;
	YMAX = y;
}
	/* return max x-y range */
max_x_y(xm, ym) long *xm, *ym; {
	*xm = XMAX;
	*ym = YMAX;
}
	/* set window range */
window(type, v) int type; long v; {
	switch (type) {

	caseof(WXL): v += base_x; XLO = v < 0 ? 0: v; break;
	caseof(WXH): v += base_x; XHI = v > XMAX ? XMAX: v; break;
	caseof(WYL): v += base_y; YLO = v < 0 ? 0: v; break;
	caseof(WYH): v += base_y; YHI = v > YMAX ? YMAX: v;
	}
}
	/* flag data v outside window */
outside (x, y) long x, y; {
	register c;

	c = (x < XLO ? LEFT : (x > XHI ? RIGHT : 0));
	if (y < YLO) return (c | BOTTOM);
	if (y > YHI) return (c | TOP);
	return (c);
}
	static int
direct = 0,
(*edger)() = outside;
	/* clip line to range inside of window */
clip(penup, x0, y0, x1, y1) long x0, y0, x1, y1; {
	register c;
	int cs, c0, c1;
	long x, y;

	cs = c0 = (*edger)(x0, y0);
	c1 = (*edger)(x1, y1);
	while (c0 | c1) {
		if (c0 & c1) return;
		c = c0 != 0 ? c0 : c1;
		if (c & LEFT) {
			y = y0 + lrnd(((double)(y1-y0))*(XLO-x0)/(x1-x0));
			x = XLO;
		}
		else if (c & RIGHT) {
			y = y0 + lrnd(((double)(y1-y0))*(XHI-x0)/(x1-x0));
			x = XHI;
		}
		else if (c & BOTTOM) {
			x = x0 + lrnd(((double)(x1-x0))*(YLO-y0)/(y1-y0));
			y = YLO;
		}
		else if (c & TOP) {
			x = x0 + lrnd(((double)(x1-x0))*(YHI-y0)/(y1-y0));
			y = YHI;
		}
		if (c == c0) {
			x0 = x; y0 = y;
			c0 = (*edger)(x, y);
		}
		else {
			x1 = x; y1 = y;
			c1 = (*edger)(x, y);
		}
	}
	if (direct) {
		if (penup || cs) (*device)(D_MOVE, x0, y0);
		(*device)(D_LINE, x1, y1);
	} else {
		if (penup || cs) (*pen->line)(-1, x0, y0);
		(*pen->line)(0, x1, y1);
	}
}
	static /* flag character data outside window */
coutside (x, y) long x, y; {
	register c;

	c = (x < 0 ? LEFT : (x > XMAX ? RIGHT : 0));
	if (y < 0) return (c | BOTTOM);
	if (y > YMAX) return (c | TOP);
	return (c);
}
	/* clip character line to inside of plotter window */
cdraw(p, xp, yp, xb, yb) char *p; short *xp, *yp; long xb, yb; {
	static int c, pen;
	static long x0, y0, x1, y1;

	edger = coutside; direct = 1;
	while ((c = *p++) > 0) {
		x1 = *xp++ + xb;
		y1 = *yp++ + yb;
		if (c != D_MOVE) {
			clip(pen, x0, y0, x1, y1);
			pen = 0;
		} else
			pen = 1;
		x0 = x1;
		y0 = y1;
	}
	edger = outside; direct = 0;
}
