#ifndef lint
static char *SCCSID = "@(#)polycheck.c	OEMG v.1.2";
#endif
# include "gen.h"
#define CROSS(a,b,c) c.A=(double)a.B*b.C-(double)a.C*b.B;\
	c.B=(double)a.C*b.A-(double)a.A*b.C; c.C=(double)a.A*b.B-(double)a.B*b.A
#define LDOT(a,b) a.x*b.A+a.y*b.B+b.C
	static long
lsquare(x,y) long x, y; { return(x*x + y*y); }
	struct
CUTS {
	IXY p;
	long r;
	int tag;
};
	static int
cmp(a, b) struct CUTS *a, *b; {
	return (a->r - b->r);
}
	void
polycheck(p0, p1, limits) IXY p0, p1; struct LIMITS *limits; {
	GAM g;
	struct { double A, B, C; } G;
	IXY p;
	long r;
	int n, i, kany, k0, k1, j0, j1;
	struct CUTS cuts[100];

	LINE(p0, p1, g);
	for (kany = n = i = 0; i < limits->count; ++i) {
		j0 = LDOT(limits->L[i].p, g); /* bdy. points side of vector */
		j1 = LDOT(limits->L[i+1].p, g);
		j0 = j0 > 0 ? 1 : ((j0 < 0 || j1 > 0) ? -1 : 1);
		j1 = j1 > 0 ? 1 : (j1 < 0 ? -1 : -j0);
		if (j0 == j1)
			continue;
		k0 = LDOT(p0, limits->L[i].g); /* side of boundary line points on */
		k1 = LDOT(p1, limits->L[i].g);
		k0 = k0 > 0 ? 1 : ((k0 < 0 || k1 > 0) ? -1 : 1);
		k1 = k1 > 0 ? 1 : (k1 < 0 ? -1 : -k0);
		if (k0 == k1) { /* bdy side pts on same side */
			kany += k0;
			continue;
		}
		CROSS(g, limits->L[i].g, G);
		p.x = lrnd(G.A / G.C);
		p.y = lrnd(G.B / G.C);
		cuts[n].r = lsquare(p.x-p0.x, p.y-p0.y);
		cuts[n].p = p;
		cuts[n++].tag = k0;
	}
	if (!n) {
		if (kany == 2)
			return;
		(*limits->clip)(p0, p1, limits->next);
	} else {
		if (n == 1)
			if (cuts[0].tag < 0)
				(*limits->clip)(p0, cuts[0].p, limits->next);
			else
				(*limits->clip)(cuts[0].p, p1, limits->next);
		else {
			IXY pl;

			qsort(cuts, n, sizeof(struct CUTS), cmp);
			pl = p0;
			for (i = 0; i < n; ++i) {
				if (cuts[i].tag < 0)
					(*limits->clip)(pl,cuts[i].p,
						limits->next);
				else
					pl = cuts[i].p;
			}
			if (cuts[n-1].tag > 0)
				(*limits->clip)(pl, p1, limits->next);
		}
	}
}
