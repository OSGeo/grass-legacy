#ifndef lint
static char *SCCSID = "@(#)xwindow.c	OEMG v.1.1";
#endif
#include "gen.h"
	static int
count = 0;
	static IXY
lo, hi,
in[MAX_POLY+1];
	void
xwindow(argc) {
	double atof();
	char *s;
	IXY p;

	if (argc < 2) return;	/* no coordinates */
	if (!*(s = setfield(0))) return;
	p.x = lrnd(atof(s) * cts_cm); resetf();
	if (!*(s = setfield(1))) return;
	p.y = lrnd(atof(s) * cts_cm); resetf();
	if (p.x < 0 || p.y < 0 || p.x > x_board || p.y > y_board)
		emess(1,"input window data off board");
	if (count >= MAX_POLY)
		emess(1,"more than %d window points",MAX_POLY);
	if (!count || p.x != in[count].x || p.y != in[count].y) {
		if (count) {
			if (p.x < lo.x) lo.x = p.x;
			else if (p.x > hi.x) hi.x = p.x;
			if (p.y < lo.y) lo.y = p.y;
			else if (p.y > hi.y) hi.y = p.y;
		} else
			lo = hi = p;
		in[count++] = p;
	}
}
	void
makewindow() {
	char *malloc();
	int i;
	long siz;
	struct LIMITS *last, *new = (struct LIMITS *)0;

	if (count <= 1)
		emess(-1,"only %d exclusion window points given\n",count);
	if (lo.x == hi.x || lo.y == hi.y)
		emess(-1,"exclusion window with 0 area\n");
	siz = sizeof(struct LIMITS);
	if (count > 2) {
		if (in[count-1].x != in[0].x && in[count-1].y != in[0].y)
			in[count++] = in[0]; /* close it up */
		siz += count * sizeof(struct GAMMA);
	}
	if (!(new = (struct LIMITS *)malloc(siz)))
		emess(1,"window allocation failure");
	new->min = lo;
	new->max = hi;
	if (count > 2) {
		new->count = --count;
		for (i = 0; i < count; ++i) {
			new->L[i].p = in[i];
			LINE(in[i],in[i+1],new->L[i].g);
		}
		new->L[i].p = in[i];
	} else
		new->count = 0;
	if (new) {
		for (last = &base_lim; last->next ; last = last->next) ;
		last->next = new;
		new->clip = last->clip;
		last->clip = new->count ? polyclip:invclip;
	}
	count = 0;
}
