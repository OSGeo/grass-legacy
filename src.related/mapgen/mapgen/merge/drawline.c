#ifndef lint
static char *SCCSID = "@(#)drawline.c	OEMG v.1.2";
#endif
#include <graphics.h>
#include "gen.h"
	void
drawline(p1, p2, limits) IXY p1, p2; struct LIMITS *limits; {

	if (!PEQ(pl, p1))
		moveto(p1.x, p1.y);
	lineto(p2.x, p2.y);
	pl = p2;
}
