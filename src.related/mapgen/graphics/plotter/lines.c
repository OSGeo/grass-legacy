#ifndef lint
static char *SCCSID = "@(#)lines.c	USGS v.4.2";
#endif
/*#include <limits.h>*/
#define LONG_MAX 2147483647L
/* solid and dashed line control */
#define PLOTTER
#include "plotter.h"
#include "graphics.h"
	extern XY *
(*device)();
	extern BASE
base;
	extern int
error;
	extern long
lrnd();
	extern PEN *
pen;
	void
solid(penup, x, y) int penup; long x, y; {
	(*device)((penup ? D_MOVE : D_LINE), x, y);
}
	static
setarray(mask, a, size) long mask; short a[]; int size; {
	int i, f, p;

	mask &= 0xffff;
	if (mask == 0 || mask == 0xffff)
		return (-(error = E_MASK));

		/* rotate so '1' in LSB */
	while (!(mask & 1)) mask >>= 1;

	p = -1;
	for (f = i = 0; i < 16; i++) {
		if ((mask & 1) == f) a[p] += size;
		else { a[++p] = size; f ^= 1; }
		mask >>= 1;
	}
	return(p);
}
	/* set size of dash element */
setdsize(v) long v; {
	if (v <= 0) return (error = E_DSIZE);
	if (!pen->dmask) pen->dmask = 0xff; /* set a default */
	pen->dsize = v;
	return(pen->damax = setarray((long)pen->dmask,pen->darray,pen->dsize));
}
	/* set mask */
setdmask(v) long v; {
		/* set default */
	if (pen->dsize <= 0) pen->dsize = 1;

	if ((pen->damax = setarray(v, pen->darray, pen->dsize)) <= 0)
		pen->dmask = 0;
	else
		pen->dmask = v;
	return(pen->damax);
}
	void
dash(penup, x, y) long x, y; {
	static long x0, y0;
	double m, dx, dy, rd, hypot();
	long t, l;

	if (!penup) {
		dx = x - x0; dy = y - y0;
		if (dx || dy) {
			l = lrnd( rd = hypot(dx, dy) );
			rd = 1. / rd;
			for (t = pen->dresid; t < l; t += pen->dresid) {
				m = t * rd;
				(*device)(pen->dindex & 1 ? D_MOVE : D_LINE,
					lrnd(x0 + m * dx), lrnd(y0 + m * dy));
				pen->dresid = pen->darray[pen->dindex =
					pen->dindex >= pen->damax ? 0 :
					pen->dindex + 1];
			}
			if (!(pen->dindex & 1)) (*device)(D_LINE, x, y);
			if ((pen->dresid = t - l) < 0)
				pen->dresid = pen->darray[pen->dindex =
				pen->dindex>=pen->damax ? 0 : pen->dindex + 1];
		}
	} else if (!(pen->dindex & 1)) (*device)(D_MOVE, x, y);
	x0 = x;
	y0 = y;
}
	/* fancy line plotting section */
	static STABLE
T;
flset(s) char *s; {
	int c;

	for (pen->f_nosym = 0; pen->f_nosym < MAX_F_SYMS
		&& (c = *s++) ; ++pen->f_nosym)
		pen->f_syms[pen->f_nosym] = c;
	pen->f_cycle = pen->f_rdist = 0;
	pen->f_pline &= ~_FLIP;
}
fline(penup, x, y) long x, y; {
	static long x0, y0;
	long l, xt, yt, t, lrnd(), hs;
	static int skip;
	int ch;
	double dx, dy, rd, m, hypot(), c, s, size;

	size = .0625 * pen->f_size;
	if (!penup && !skip) {
		dx = x - x0; dy = y - y0;
		if (dx || dy) {
			l = lrnd( rd = hypot(dx, dy) );
			rd = 1. / rd;
			for (t = 0; t <= l; ) {
				if ( !pen->f_rdist ) {
					m = t * rd;
					pen->cflags |= C_SYM;
					ch = pen->f_syms[pen->f_cycle];
					if (ch & 0x80)
						pen->f_pline ^= _FLIP;
					s = size * (pen->f_pline
						& _FLIP ? -rd : rd);
					c = dx * s;
					s *= dy;
					if (setchar(ch, &T, size, c, s, 1)) {
						xt =x0 + lrnd(dx * m),
						yt =y0 + lrnd(dy * m);
						cdraw(T.pen, T.x, T.y, xt, yt);
						if (pen->f_pline & _FLINE)
							(*pen->dsline)(1,xt,yt);
						hs = T.r;
					} else
						hs = 0;
					pen->cflags &= ~C_SYM;
					do {
						if(++pen->f_cycle>=pen->f_nosym)
							{
							pen->f_cycle = 0;
							pen->f_pline &= ~_FLIP;
							}
						ch = pen->f_syms[pen->f_cycle];
						if (skip = ch == '\377')
							hs = LONG_MAX/2;
						pen->f_rdist+= pen->f_dist + hs
							+ getlsize(ch, size);
					} while (!skip && (pen->f_syms[pen->f_cycle]
						& 0x7f) == 0x7f);
				}
				if (( t += pen->f_rdist ) <= l) {
					pen->f_rdist = 0;
					m = t * rd;
					if (t == l) { xt = x; yt = y; }
					else {
						xt = x0 + lrnd(dx * m);
						yt = y0 + lrnd(dy * m);
					}
				} else {
					xt = x; yt = y;
					pen->f_rdist = t - l;
				}
				if (pen->f_pline & _FLINE)
					(*pen->dsline)(0, xt, yt);
			}
		}
	} else {
		if (penup) {
			skip = 0;
			pen->f_cycle = pen->f_rdist = 0;
			pen->f_pline &= ~_FLIP;
		}
		if (pen->f_pline & _FLINE)
			(*pen->dsline)(penup, x, y);
	}
	x0 = x;
	y0 = y;
}
