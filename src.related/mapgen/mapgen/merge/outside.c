#ifndef lint
static char *SCCSID = "@(#)outside.c	OEMG v.1.1";
#endif
#include "gen.h"
	int /* flag data v outside window */
outside(p, limits) IXY p; struct LIMITS *limits; {
	register c;

	c = (p.x < limits->min.x ? LEFT : (p.x > limits->max.x ? RIGHT : 0));
	if (p.y < limits->min.y) return (c | BOTTOM);
	if (p.y > limits->max.y) return (c | TOP);
	return (c);
}
