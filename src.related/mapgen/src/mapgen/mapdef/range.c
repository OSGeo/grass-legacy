#ifndef lint
static char *SCCSID = "@(#)range.c	AMG v.3.1";
#endif
/* range - determine centimetric range of basic geographic area
** determine range of projection area by "rhumb" lining the geographic
** edges of the map.  This is not the most elegant, but serves the
** purpose and it is probable not worth the effort to develope a real
** "min-max" routine for this infrequently employed procedure
*/
# include <setjmp.h>
# include <stdio.h>
# include <math.h>
# include "mapgen.h"
# include "mapdef.h"
# define R_TOL .1
typedef struct {
	double l, p, x, y;
} P;
	static
jmp_buf jmpret;
	static
level;
	static double
xlo, xhi, ylo, yhi;
	static
minmax(x, y) double x, y; {
	if (x < xlo) xlo = x;
	else if (x > xhi) xhi = x;
	if (y < ylo) ylo = y;
	else if (y > yhi) yhi = y;
}
	static
rhumbr(p0, p1) P *p0, *p1; {
	P pm;

	pm.l = (p0->l + p1->l) * .5;
	pm.p = (p0->p + p1->p) * .5;
	if (mproj(pm.l, pm.p, &pm.x, &pm.y)) longjmp(jmpret, 0);
	if (! level++ || fabs((p0->y + p1->y) * .5 - pm.y) > R_TOL ||
			fabs((p0->x + p1->x) * .5 - pm.x) > R_TOL) {
		rhumbr(p0, &pm);
		rhumbr(&pm, p1);
	}
	--level;
	minmax(p1->x, p1->y);
}
	static
rhumbs(x0, y0, x1, y1) double x0, y0, x1, y1; {
	static P a, b;

	level = 0;
	if (mproj(x0, y0, &a.x, &a.y)) longjmp(jmpret, 0);
	minmax(a.x, a.y);
	if (mproj(x1, y1, &b.x, &b.y)) longjmp(jmpret, 0);
	a.l = x0;
	a.p = y0;
	b.l = x1;
	b.p = y1;
	rhumbr(&a, &b);
}
range(xmin, xmax, ymin, ymax) double *xmin, *xmax, *ymin, *ymax; {
		/* in case 'mproj' blows in recursion, provide quick out */
	if (setjmp(jmpret))
		return(1); /* also tell the caller about it */
		/* initialize range values */
	xlo = ylo = HUGE;
	xhi = yhi = -HUGE;
		/* do rhumb line of each side */
	rhumbs(def.l_lon, def.b_lat, def.l_lon, def.t_lat); /* left */
	rhumbs(def.l_lon, def.t_lat, def.r_lon, def.t_lat); /* top */
	rhumbs(def.r_lon, def.b_lat, def.r_lon, def.t_lat); /* right */
	rhumbs(def.l_lon, def.b_lat, def.r_lon, def.b_lat); /* bottom */
	*xmin = xlo - R_TOL;
	*xmax = xhi + R_TOL;
	*ymin = ylo - R_TOL;
	*ymax = yhi + R_TOL;
	return (0);
}
