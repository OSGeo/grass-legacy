#ifndef lint
static char *SCCSID = "@(#)scalein.c	OEMG v.1.1";
#endif
/* initialize structure and plotter */

# include <stdio.h>
# include <math.h>
#include <plotgen.h>
#include <mapgen.h>
	extern struct PLTDEF
p_def;
	static int 
zscale(p, axis) struct AXIS *p; char *axis; {

	if (p->cmboard <= 0.) p->cmboard = p->wmax + 1.;
	if (p->wmin <0 || p->wmin >= p->wmax) {
		emess("invalid window values for %s axis",axis);
		return(1);
	}
	if (p->dmin == p->dmax) {
		emess("data limits equal for %s axis", axis);
		return(1);
	}
	if (p->log) {
		if (p->dmin <= 0. || p->dmax <= 0.) {
			emess("range <= 0. for %s log axis", axis);
			return(1);
		}
		p->dmin = log10((double)p->dmin);
		p->dmax = log10((double)p->dmax);
	}
	p->scale = p_def.cts * (p->wmax - p->wmin) / (p->dmax - p->dmin);
	p->offset = p_def.cts * p->wmin - p->scale * p->dmin;
	p->board = p_def.cts * p->cmboard + .5;
	p->min = floor((double)p_def.cts * p->wmin);
	p->max = ceil((double)p_def.cts * p->wmax);
	return(0);
}
	/* set scale data and initialize plotter */
	static int
setscale() {
	/* check out params */
	if (p_def.cts <= 0.) p_def.cts = 200.;
	return (zscale(&p_def.x,"X") || zscale(&p_def.y,"Y"));
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
	int /* get scale parameters */
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
				p_def.cts = atof(*argv++);
			else
noarg:				emess("arg -%c has no value",*arg);
				return(1);
			continue;
		case 'e':	/* done with scaling */
			return(setscale());
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
				if (type == 'x') p_def.x.cmboard = f;
				else if (type == 'y') p_def.y.cmboard = f;
				else p_def.x.cmboard = p_def.y.cmboard = f;
			} else goto noarg;
			continue;
		case 'w':	/* set window size */
			if (argc-- > 0)
				getreal(*argv++, type == 'x' ?
					&p_def.x.wmin : &p_def.y.wmin);
			else goto noarg;
			continue;
		case 'd':	/* set data range */
			if (argc-- > 0)
				getreal(*argv++, type == 'x' ?
					&p_def.x.dmin : &p_def.y.dmin);
			else goto noarg;
			continue;
		case 'l':	/* logarithmic */
			if (type == 'x')	p_def.x.log = 1;
			else if (type == 'y')	p_def.y.log = 1;
			else p_def.x.log = p_def.y.log = 1;
			continue;
		case 'p':
			if (argc-- > 0) {
				f = atof(*argv++);
				if (type == 'x') p_def.x.del = f;
				else if (type == 'y') p_def.y.del = f;
				else p_def.x.del = p_def.y.del = f;
			} else goto noarg;
			continue;
		case 'n':
			if (argc-- > 0) {
				i = atoi(*argv++);
				if (type == 'x') p_def.x.sub = i;
				else if (type == 'y') p_def.y.sub = i;
				else p_def.x.sub = p_def.y.sub = i;
			} else goto noarg;
			continue;
		default:
			emess("invalid scaling command:-%s\n",arg);
			return(1);
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
	return(1);
}
