#ifndef lint
static char *SCCSID = "@(#)setpltr.c	AMG v.3.3";
#endif
/* initialize interactive plotting for pltgen programs
*/
# include <stdio.h>
# include "plotgen.h"
# include "graphics.h"

extern struct PLTDEF def;

static char *argl[] = {0, 0,"-i",".", 0};
	static void
setpltr(clear) {
	ANSWR *ans;
	char line[20];
	double fx, fy;

		/* link to graphics */
	if (plotopen(argl))
		emess(1,"graphics device open failure", (char *)0);
		/* get size of plotter */
	ans = (ANSWR *)plotreq(P_SIZE);
	fx = (double) ans->x / def.x.board;
	fy = (double) ans->y / def.y.board;
	if (fx > fy)
		fx = fy;
	sprintf(line, "%e", fx);
	plotopt(RESCALE, line);
		/* clear screen */
	if(clear)
		plotopt(ERASE);
}
	void /* initialize plotter */
setplot() {
	if (def.inter)
		setpltr(1);	/* initialize plotter */
	if (def.overlay && defopen(def.overlay)) /* open defered file */
		emess(2,"overlay openning failure", (char *)0);
	init();
}
