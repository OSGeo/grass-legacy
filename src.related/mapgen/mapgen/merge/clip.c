#ifndef lint
static char *SCCSID = "@(#)clip.c	OEMG v.1.1";
#endif
#include "gen.h"
	void /* clip line to range inside of window */
clip(p1, p2, limits) IXY p1, p2; struct LIMITS *limits; {
	register c;
	int c1, c2;
	IXY p;

	c1 = outside(p1, limits);
	c2 = outside(p2, limits);
	while (c1 | c2) {
		if (c1 & c2) return;
		c = c1 != 0 ? c1 : c2;
		if (c & LEFT) {
			p.y = p1.y + F(p2.y,p1.y,limits->min.x,p1.x,p2.x);
			p.x = limits->min.x;
		} else if (c & RIGHT) {
			p.y = p1.y + F(p2.y,p1.y,limits->max.x,p1.x,p2.x);
			p.x = limits->max.x;
		} else if (c & BOTTOM) {
			p.x = p1.x + F(p2.x,p1.x,limits->min.y,p1.y,p2.y);
			p.y = limits->min.y;
		} else if (c & TOP) {
			p.x = p1.x + F(p2.x,p1.x,limits->max.y,p1.y,p2.y);
			p.y = limits->max.y;
		}
		if (c == c1)	c1 = outside(p1 = p, limits);
		else		c2 = outside(p2 = p, limits);
	}
	(*limits->clip)(p1, p2, limits->next);
}
