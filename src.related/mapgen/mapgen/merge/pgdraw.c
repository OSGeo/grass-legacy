#ifndef lint
static char *SCCSID = "@(#)pgdraw.c	OEMG v.1.2";
#endif
#include "gen.h"
	static
IXY p1, p2;
	void
pgdraw(argc) {
	double x, y, atof(), log10();
	char *s;
	int i;

	if (argc < 2) return;	/* no coordinates */

	if (!*(s = setfield(0))) return;
	x = atof(s); resetf();
	if (!*(s = setfield(1))) return;
	y = atof(s); resetf();
	if (p_def.x.log) {
		if (x <= 0.) {
neglog:			emess(-1,"negative log mode value dropped\n");
			return;
		} else x = log10(x);
	}
	if (p_def.y.log) {
		if (y <= 0.) goto neglog;
		else y = log10(y);
	}
	if (!oldline) {
		p1.x = lrnd(x * p_def.x.scale + p_def.x.offset);
		p1.y = lrnd(y * p_def.y.scale + p_def.y.offset);
		oldline = 1;
	} else {
		p2.x = lrnd(x * p_def.x.scale + p_def.x.offset);
		p2.y = lrnd(y * p_def.y.scale + p_def.y.offset);
		clip(p1, p2, &base_lim);
		p1 = p2;
	}
}
