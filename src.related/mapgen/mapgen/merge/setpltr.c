#ifndef lint
static char *SCCSID = "@(#)setpltr.c	OEMG v.1.1";
#endif
/* initialize interactive plotting for mapgen programs */
# include <stdio.h>
# include <graphics.h>
# include "gen.h"

static char *argl[] = {0, 0,"-i",".", 0};

setpltr(clear) {
	ANSWR *ans;
	char line[20];
	double fx, fy;

		/* link to graphics */
	if (plotopen(argl)) emess(1,"graphics device open failure");
		/* get size of plotter */
	ans = (ANSWR *)plotreq(P_SIZE);
	fx = (double) ans->x / x_board;
	fy = (double) ans->y / x_board;
	if (fx > fy)
		fx = fy;
	sprintf(line, "%e", fx);
	plotopt(RESCALE, line);
		/* clear screen */
	if(clear)
		plotopt(ERASE);
	return 0;
}
