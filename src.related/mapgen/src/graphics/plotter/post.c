#ifndef lint
static char *SCCSID = "@(#)post.c	OEMG v.2.1";
#endif
/* plots character string */
# include "plotter.h"

extern PEN *pen;

static long px, py;

static STABLE t;

# define THETA		pen->cflags & C_THETA
# define STHETA		pen->cflags & C_STHETA
# define NON_LEFT	pen->cflags & (C_RIGHT + C_CENTER)
# define CENTERED	pen->cflags & C_CENTER
	extern long
lrnd();
	static float
rsize;
	static char *
vindex(font, i) FONT *font; int i; {
	unsigned short c;
	return ((c = font->dir[i]) ? font->vect + c : (char *)0);
}
	static char * /* set pointer to vector selected by 'c' */
setvect(c) int c; { char *s;
	if (!(c &= 0x7f))
		return((char *)0);
	if (C_SYM & pen->cflags && pen->sfont) { /* symbol font */
		rsize = pen->sfont->rsize;
		return(vindex(pen->sfont, c));
	} else if (C_AFONT & pen->cflags)
		if (pen->afont && (s = vindex(pen->afont, c))) {
			rsize = pen->afont->rsize;
			return (s);	/* alternate font */
		}
		/* primary font */
	rsize = pen->font->rsize;
	return(pen->font ? vindex(pen->font, c) : (char *)0);
}
	/* return plotting length of character string */
	long
slength(s) char *s; {
	int c, t; char *p;
	float len = 0;
	len = 0;
	t = pen->cflags;

	while ((c = (*s++ & 0x7f)) && c != '\n')
		if (c == ACHAR)
			pen->cflags |= C_AFONT;
		else if (c == PCHAR)
			pen->cflags &= ~C_AFONT;
		else if (p = setvect(c))
			len += (p[1]-p[0]) * rsize;
	pen->cflags = t;
	return ((long)(pen->csize * len + 8.) >> 4);
}
	/* setup scaled character vectors */
setchar(c, table, size, cos, sin, theta)
int c; STABLE *table; int theta; double size, cos, sin; {
	register char *p;
	char *pu;
	int penup;
	short *xp, *yp;
	register x, y;
 
	if (p = setvect(c)) {
		size *= rsize;
		if (theta) {
			cos *= rsize;
			sin *= rsize;
		}
		penup = D_MOVE;
		pu = table->pen;
		xp = table->x;
		yp = table->y;
		table->l = lrnd(*p++ * size);
		table->r = lrnd(*p++ * size);
		p+=2;
		for (x = *p++; (y = *p++) != -128 ; x = *p++)
			if (x != -128) {
				if (theta) {
					*xp++ = lrnd(cos * x - sin * y);
					*yp++ = lrnd(sin * x + cos * y);
				} else {
					*xp++ = lrnd(size * x);
					*yp++ = lrnd(size * y);
				}
				*pu++ = penup;
				penup = D_LINE;
			} else
				penup = D_MOVE;
		*pu = -1;
		return (1);
	}
	return (0);
}
	void /* post a string */
post(str) char *str; {
	int c,theta;
	long lx, bx, by, sx, sy;

	if (outside(X, Y)) return;

	theta = THETA;
	sx = X + pen->r_xoff;
	sy = Y + pen->r_yoff;

	while (c = (*str++ & 0x7f)) {
		if (c == '\n') {
			if (pen->font) {
				lx = (pen->font->vect[0] *
					pen->csize * pen->leading + 64) >> 7;
				if (theta) {
					sx += lx * pen->sint;
					sy -= lx * pen->cost;
				} else
					sy -= lx;
			}
			continue;
		}
		bx = sx; by = sy;
		if (NON_LEFT) {
			lx = slength(str);
			if (CENTERED)
				lx /= 2;
			if (theta) {
				bx -= lx * pen->cost;
				by -= lx * pen->sint;
			} else
				bx -= lx;
		}
		lx = 0;
		do {
			if (c == ACHAR) {
				pen->cflags |= C_AFONT;
				continue;
			}
			if (c == PCHAR) {
				pen->cflags &= ~C_AFONT;
				continue;
			}
			if (!setchar(c, &t, .0625 * pen->csize, pen->bcost,
					pen->bsint, theta)){
				continue;}
			if (lx) {
				lx -= t.l;
				if (theta) {
					px = bx + lx * pen->cost;
					py = by + lx * pen->sint;
				} else {
					px = bx + lx;
					py = by;
				}
			} else {
				px = bx;
				py = by;
			}
			lx += t.r;
			cdraw(t.pen, t.x, t.y, px, py);
		} while ((c = (*str++ & 0x7f)) && c != '\n');
		str--;
	}
}
	/* symbol plotting procedure */
symbol() {
	if (outside(X, Y)) return;
	pen->cflags |= C_SYM;
	if (setchar(pen->sym, &t, .0625 * pen->ssize, pen->sbcost,
		pen->sbsint, STHETA) )
		cdraw(t.pen, t.x, t.y, X, Y);
	pen->cflags &= ~C_SYM;
}
