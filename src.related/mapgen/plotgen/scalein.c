#ifndef lint
static char *SCCSID = "@(#)scalein.c	AMG v.3.3";
#endif
/* initialize structure and plotter */

# include <stdio.h>
# include <math.h>
# include "plotgen.h"
	extern struct PLTDEF
def;
	static void 
zscale(p, axis) struct AXIS *p; char *axis; {

	if (p->cmboard <= 0.) p->cmboard = p->wmax + 1.;
	if (p->wmin <0 || p->wmin >= p->wmax)
		emess(1, "invalid window values for",axis,"axis", (char *)0);
	if (p->dmin == p->dmax)
		emess(1, "data limits equal for",axis,"axis", (char *)0);
	if (p->log) {
		if (p->dmin <= 0. || p->dmax <= 0.)
			emess(1, "range <= 0. for",axis,"log axis", (char *)0);
		p->dmin = log10((double)p->dmin);
		p->dmax = log10((double)p->dmax);
	}
	p->scale = def.cts * (p->wmax - p->wmin) / (p->dmax - p->dmin);
	p->offset = def.cts * p->wmin - p->scale * p->dmin;
	p->board = def.cts * p->cmboard + .5;
	p->min = floor((double)def.cts * p->wmin);
	p->max = ceil((double)def.cts * p->wmax);
}
	/* set scale data and initialize plotter */
setscale() {
	/* check out params */
	if (def.cts <= 0.) def.cts = 200.;
	zscale(&def.x,"X");
	zscale(&def.y,"Y");
	setplot();
}
	static void /* get double float words */
getreal(s, v) char *s; float *v; {
	char *strchr();

	*v++ = atof(s);
	if (s = strchr(s, ','))
		*v = atof(++s);
	else
		*v = 0.;
}
	/* get scale parameters */
scalein(argc, argv) char **argv; int argc; {
	int type, c, i;
	double f;
	char *arg;

	while (argc-- > 0) if ((c = **argv) == '-') for (arg = *argv++;;) {
		switch (*++arg) {
		case '\0':
			break;
		case 'q':	/* set counts / cm. */
			if (argc-- > 0)
				def.cts = atof(*argv++);
			continue;
		case 'e':	/* done with scaling */
			if (! def.defer)
				setscale();
			return(0);
		case 'x':	/* select axis */
		case 'y':
			if (arg[-1] == '-')
				type = *arg;
			else
				type = 0;
			continue;
		case 'b':	/* set board size */
			if (argc-- > 0) {
				f = atof(*argv++);
				if (type == 'x') def.x.cmboard = f;
				else if (type == 'y') def.y.cmboard = f;
				else def.x.cmboard = def.y.cmboard = f;
			}
			continue;
		case 'w':	/* set window size */
			if (argc-- > 0)
				if (type == 'x')
					getreal(*argv++, &def.x.wmin);
				else {
					getreal(*argv++, &def.y.wmin);
					if (type != 'y') {
						def.x.wmin = def.y.wmin;
						def.x.wmax = def.y.wmax;
					}
				}
			continue;
		case 'd':	/* set data range */
			if (argc-- > 0)
				if (type == 'x')
					getreal(*argv++, &def.x.dmin);
				else {
					getreal(*argv++, &def.y.dmin);
					if (type != 'y') {
						def.x.dmin = def.y.dmin;
						def.x.dmax = def.y.dmax;
					}
				}
			continue;
		case 'l':	/* logarithmic */
			if (type == 'x')	def.x.log = 1;
			else if (type == 'y')	def.y.log = 1;
			else def.x.log = def.y.log = 1;
			continue;
		case 'p':
			if (argc-- > 0) {
				f = atof(*argv++);
				if (type == 'x') def.x.del = f;
				else if (type == 'y') def.y.del = f;
				else def.x.del = def.y.del = f;
			}
			continue;
		case 'n':
			if (argc-- > 0) {
				i = atoi(*argv++);
				if (type == 'x') def.x.sub = i;
				else if (type == 'y') def.y.sub = i;
				else def.x.sub = def.y.sub = i;
			}
			continue;
		default:
			emess(0,"invalid scaling command",arg, (char *)0);
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
	return(1);
}
