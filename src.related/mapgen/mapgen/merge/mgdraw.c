#ifndef lint
static char *SCCSID = "@(#)mgdraw.c	OEMG v.1.2";
#endif
#include "gen.h"
# define iabs(x) ((x) < 0 ? -(x) : (x))
# define isign(x, y) ((y) < 0 ? -(x) : (x))
# define IPI		314159265
# define ITWO_PI	628318531
	/* integer reduction of longitude to +/- pi range */
#define ADJ(x) x -= icm; while (iabs(x) > IPI) x -= isign(ITWO_PI, x); x += icm
	static IXY
cl1, c1, c2, p1, p2, pt,
pl1 = {ITWO_PI,ITWO_PI};
	void /* input geographic coordinates and prep long lon-lat vector */
mgdraw(argc) {
	double dmstor();
	char *s;
	long delx;

	if (argc >= 2) { /* two points? */
		if (!*(s = setfield(0))) return;
		p2.x = DI_CON * dmstor(s, 0); resetf();
		ADJ(p2.x);	/* reduce longitude range */
		if (!*(s = setfield(1))) return;
		p2.y = DI_CON * dmstor(s, 0); resetf();
		if (!oldline)
			oldline = 1;
		else if (iabs(delx = p2.x - p1.x) > IPI) { /* wrap around */
			pt = p2; pt.x -= (delx = isign(ITWO_PI, delx));
			clip(p1, pt, &geog_lim);
			pt = p1; pt.x += delx;
			clip(pt, p2, &geog_lim);
		} else
			clip(p1, p2, &geog_lim);
		p1 = p2;
	}
}
	static void /* project long lon-lat to board units */
iproj(LL, p) IXY LL, *p; {
	double px, py;

	approj(ID_CON * LL.x, ID_CON * LL.y, &px, &py);
	p->x = lrnd(m_def.cosr * px - m_def.sinr * py + m_def.xf_off);
	p->y = lrnd(m_def.sinr * px + m_def.cosr * py + m_def.yf_off);
}
	void /* project windowed geographic vector */
mgproj(p1, p2, next) IXY p1, p2; struct LIMITS *next; {
	if (PEQ(p1,pl1)) /* likely last p2 is same for current p1 */
		c1 = cl1; /* so, save a little time */
	else
		iproj(p1, &c1);
	iproj(p2, &c2);
	clip(c1, c2, next);
	pl1 = p2;
	cl1 = c2;
}
