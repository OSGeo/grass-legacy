#ifndef lint
static char *SCCSID = "@(#)adjcoef.c	AMG v.3.1";
#endif
/* Trim coefficient arrays to values pertinent to precision
** required.  Once done, however, there's no return.
*/
# include "mapgen.h"

extern struct map_def def;

adjcoef(mincoef)
double mincoef;
{
	int n, i, j;
	double fabs();

	def.x_deg = def.y_deg = 0;
	for (n = i = 0; i <= MAX_T_DEG ; i++)
		for (j = 0; j <= i; j++) {
			if (fabs(def.xC[n]) < mincoef)
				def.xC[n] = 0.;
			else if (i > def.x_deg) def.x_deg = i;
			if (fabs(def.yC[n]) < mincoef)
				def.yC[n] = 0.;
			else if (j > def.y_deg) def.y_deg = j;
			++n;
		}

	if (def.y_deg > def.x_deg) def.x_deg = def.y_deg;
}
