#ifndef lint
static char *SCCSID = "@(#)post.c	USGS v.4.3";
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
vindex(font, i) FONT *font; {
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
	if (pen->font) { /* set ? */
		rsize = pen->font->rsize;
		return(vindex(pen->font, c));
	} else {
		rsize = 0.;
		return((char *)0);
	}
}
	static long /* return plotting length of character string */
slength(s) char *s; {
	int c, save_cflags, first = 1;
	char *p = (char *)0;
	int len = 0;

	save_cflags = pen->cflags;
	while ((c = (*s++ & 0x7f)) && c != '\n')
		if (c == ACHAR)
			pen->cflags |= C_AFONT;
		else if (c == PCHAR)
			pen->cflags &= ~C_AFONT;
		else if (p = setvect(c))
			if (first) { /* right half of 1st char */
				len = p[1];
				first = 0;
			} else
				len += (p[1]-p[0]);
	if (p) /* remove right half of last char */
		len -= p[1];
	pen->cflags = save_cflags;
	return ((long)(pen->csize * len * rsize * 0.0625 + 0.5));
}
	int /* return left size of symbol character */
getlsize(c, size) int c; double size; {
	char *p;
	int result;

	pen->cflags |= C_SYM;
	if (p = setvect(c)) {
		size *= rsize;
		result = -lrnd(*p * size);
	} else
		result = 0;
	pen->cflags &= ~C_SYM;
	return(result);
}
	int /* setup scaled character vectors */
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
	int c, theta;
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
			lx = slength(str-1);
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
					pen->bsint, theta))
				continue;
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
	void /* symbol plotting procedure */
symbol() {
	if (outside(X, Y)) return;
	pen->cflags |= C_SYM;
	if (setchar(pen->sym, &t, .0625 * pen->ssize, pen->sbcost,
		pen->sbsint, STHETA) )
		cdraw(t.pen, t.x, t.y, X, Y);
	pen->cflags &= ~C_SYM;
}
