#ifndef lint
static char *SCCSID = "@(#)georange.c	AMG v.3.2";
#endif
/* principle windowing procedures */
# include <setjmp.h>
# include "graphics.h"
# include "mapgen.h"

static jmp_buf jmpret;
extern void longjmp();

static vector();

extern struct map_def def;

/* edge intercept flags for vector data */
int	edge_flag;
int	edge0, edge1;

mdraw(pen, x, y) long x, y; { pxyxmit(pen ? _PENUP : 0, x, y); return (0); }
mpoint(x, y) long x, y; { pxyxmit(_PENUP, x, y); return (0); }
	struct srange
srange = {0, 0, mdraw, mpoint};
	static
struct RANGE {
	int	sflag, eflag;
	long	xlo, xhi, ylo, yhi;
} geog, cart;

static int	pen;	/* current pen status */
static int	level1;	/* windowing level */
static long	xl, yl;
static int	carts;
static long	icm, icp;
static int	rhumbf;

#define XLO range->xlo
#define XHI range->xhi
#define YLO range->ylo
#define YHI range->yhi

# define iabs(x) ((x) < 0 ? -(x) : (x))
# define isign(x, y) ((y) < 0 ? -(x) : (x))
# define IPI		314159265
# define ITWO_PI	628318531
	static long /* integer reduce longitude to +/- pi range */
ADJ(x) long x; {
	x -= icm;
	while (iabs(x) > IPI) x -= isign(ITWO_PI, x);
	return (icm + x);
}
	static /* flag outside window */
outside (x, y, range) long x, y; struct RANGE *range; {
	int c;

	c = (x < XLO ? LEFT : (x > XHI ? RIGHT : 0));
	if (y < YLO) return (c | BOTTOM);
	if (y > YHI) return (c | TOP);
	return (c);
}
	static /* clip line to range inside of window */
clipper(penup, x0, y0, x1, y1, range)long x0, y0, x1, y1; struct RANGE *range;{
	register c;
	int cs, cp;
	long x, y;

	cs = srange.c0 = outside(x0, y0, range);
	srange.c1 = outside(x1, y1, range);
	range->sflag = range->eflag = 0;
	while (srange.c0 | srange.c1) {
		if (srange.c0 & srange.c1) return(srange.c1);
		c = srange.c0 ? srange.c0 : srange.c1;
		if (c & LEFT) {
			y = y0 + (long)((y1-y0)*((XLO-x0) /
				((double)(x1-x0))));
			x = XLO; cp = LEFT;
		}
		else if (c & RIGHT) {
			y = y0 + (long)((y1-y0)*((XHI-x0) /
				((double)(x1-x0))));
			x = XHI; cp = RIGHT;
		}
		else if (c & BOTTOM) {
			x = x0 + (long)((x1-x0)*((YLO-y0) /
				((double)(y1-y0))));
			y = YLO; cp = BOTTOM;
		}
		else if (c & TOP) {
			x = x0 + (long)((x1-x0)*((YHI-y0) /
				((double)(y1-y0))));
			y = YHI; cp = TOP;
		}
		if (c == srange.c0) {
			x0 = x; y0 = y;
			srange.c0 = outside(x, y, range);
			range->sflag = cp;
		} else {
			x1 = x; y1 = y;
			srange.c1 = outside(x, y, range);
			range->eflag = cp;
		}
	}
	vector(penup | cs, x0, y0, x1, y1);
	return 0;
}
typedef struct { long l, p, x, y; } P;
static int rpen;
	static
rhumbr(p0, p1) P *p0, *p1; {
	P pm;
	long delx, dely;

	pm.l = (p0->l + p1->l) / 2;
	pm.p = (p0->p + p1->p) / 2;
	iproj(pm.l, pm.p, &pm.x, &pm.y);
	delx = (p0->x + p1->x) / 2 - pm.x;
	dely = (p0->y + p1->y) / 2 - pm.y;
	if (iabs(delx) > rhumbf || iabs(dely) > rhumbf) {
		rhumbr(p0, &pm);
		rhumbr(&pm, p1);
	} else {
		clipper(rpen, p0->x, p0->y, p1->x, p1->y, &cart);
		rpen = 0;
	}
}
	static
rhumbs(penup, x0, y0, xp0, yp0) long x0, y0, xp0, yp0; {
	static P a, b;
	static P *A = &a, *B = &b, *t;

	level1 = 0;
	if (rpen = penup) {
		A->l = x0; A->p = y0;
		iproj(x0, y0, &A->x, &A->y);
	}
	B->l = xp0; B->p = yp0;
	iproj(xp0, yp0, &B->x, &B->y);
	rhumbr(A, B);
	t = A; A = B; B = t;
}
	static
vector(penup, x0, y0, x1, y1) long x0, y0, x1, y1; {
	static long xp0, yp0;
	int kill;

	if (level1) {
		if (rhumbf) {
			rhumbs(penup, x0, y0, x1, y1);
			return;
		}
		if (penup) iproj(x0, y0, &xp0, &yp0);
		iproj(x1, y1, &x1, &y1);
		if (carts) {
			level1 = 0;
			clipper(penup, xp0, yp0, x1, y1, &cart);
			xp0 = x1; yp0 = y1;
			return;
		}
	}
	kill = 0;
	edge0 = geog.sflag | cart.sflag;
	edge1 = geog.eflag | cart.eflag;
	if (penup) {
		edge_flag = (geog.sflag << 4) + cart.sflag;
		if (level1)	kill = (*srange.m_line)(1, xp0, yp0);
		else		kill = (*srange.m_line)(1, x0, y0);
	}
	edge_flag = (geog.eflag << 4) + cart.eflag;
	kill |= (*srange.m_line)(0, x1, y1);
	if (rhumbf && kill) longjmp(jmpret, 1);
	return;
}
static int no_wrap;	/* don't do wrap around */
	/* set no wrap around */
nowrap(i) { no_wrap = i; }
	/* set rhumb line mode */
rhumb(i) { rhumbf = (i <= 0 ? 0: (i < 1 ? 2: i)); }
	/* start new line */
newln(x, y) long x, y; {
	if (no_wrap)	xl = x;
	else		xl = ADJ(x);
	yl = y;
	pen = 1;
}
	/* double start new line */
newline(x, y) double x, y; { newln((long)(DI_CON * x), (long)(DI_CON * y)); }
	/* point plot motion */
point(x, y) long x, y; {
	int r;

	x = ADJ(x);
	if (! (r = outside(x, y, &geog))) {
		iproj(x, y, &x, &y);
		if (! carts || ! (r = outside(x, y, &cart)))
			(*srange.m_point)(x, y);
	}
	return (r);
}
	/* double point */
dpoint(x, y) double x, y; {
	return(point((long)(DI_CON * x), (long)(DI_CON * y)));
}
	/* continue line */
conln(x, y) long x, y; {
	long delx;

	if (rhumbf && setjmp(jmpret)) return;
	level1 = 1;
	if (no_wrap) goto clipit;
	x = ADJ(x);
	delx = x - xl;
	if (iabs(delx) > IPI) {
		delx = isign(ITWO_PI, delx);
		pen = clipper(pen, xl, yl, x - delx, y, &geog);
		pen = clipper(pen, xl + delx, yl, x, y, &geog);
	} else
clipit: 
	pen = clipper(pen, xl, yl, x, y, &geog);
	xl = x; yl = y;
	return;
}
	/* double coordinate continue line */
conline(x, y) double x, y; { conln((long)(DI_CON * x), (long)(DI_CON * y)); }
	/* determine if overlap of window */
no_lap(x0, y0, x1, y1) long x0, y0, x1, y1; {
	long delx;

	if (outside(icm, y0, &geog) & outside(icm, y1, &geog))
		return (1);
	x0 = ADJ(x0); x1 = ADJ(x1);
	delx = x0 - x1;
	if (iabs(delx) > IPI) {
		delx = isign(ITWO_PI, delx);
		return ((outside(x0, icp, &geog) &
				outside(x1 - delx, icp, &geog)) &&
			(outside(x0 + delx, icp, &geog) &
				outside(x1, icp, &geog)));
	} else
		return (outside(x0, icp, &geog) & outside(x1, icp, &geog));
}
	/* initialize */
geoinit(cart_win, line, point) int cart_win, (*line)(), (*point)(); {
	static inited = 0;

	if (line) srange.m_line = line;
	if (point) srange.m_point = point;
	carts = cart_win;
	if (! inited) {
		inited = 1;
		geog.xlo = DI_CON * def.l_lon;
		geog.xhi = DI_CON * def.r_lon;
		geog.ylo = DI_CON * def.b_lat;
		geog.yhi = DI_CON * def.t_lat;
		icm = (geog.xlo + geog.xhi) / 2;
		icp = (geog.ylo + geog.yhi) / 2;
		cart.xlo = def.D.x_min;
		cart.xhi = def.D.x_max;
		cart.ylo = def.D.y_min;
		cart.yhi = def.D.y_max;
	}
}
