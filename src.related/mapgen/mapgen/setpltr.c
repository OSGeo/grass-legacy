#ifndef lint
static char *SCCSID = "@(#)setpltr.c	AMG v.3.1";
#endif
/* initialize interactive plotting for mapgen programs
*/
# include <stdio.h>
# include "mapgen.h"
# include "graphics.h"

extern struct map_def def;

static char *argl[] = {0, 0,"-i",".", 0};

setpltr(clear) {
	ANSWR *ans;
	char line[20];
	double fx, fy;

		/* link to graphics */
	if (plotopen(argl)) emess(1,"graphics device open failure",(char *)0);

		/* get size of plotter */
	ans = (ANSWR *)plotreq(P_SIZE);
	fx = (double) ans->x / def.B.x_max;
	fy = (double) ans->y / def.B.y_max;
	if (fx > fy)
		fx = fy;
	sprintf(line, "%e", fx);
	plotopt(RESCALE, line);

		/* clear screen */
	if(clear)
		plotopt(ERASE);

	return 0;
}
