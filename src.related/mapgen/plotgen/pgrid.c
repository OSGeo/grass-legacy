#ifndef lint
static char *SCCSID = "@(#)pgrid.c	AMG v.3.1";
#endif
/* generate grid overlay */
# include <string.h>
# include <stdio.h>
# include <math.h>
# include "graphics.h"
# include "plotgen.h"

struct PLTDEF def;

main(argc, argv)
char **argv;
{
	char	*arg, **eargv;
	FILE	*fid;
	int	eargc = 0;

	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if (arg[-1] == '-')
					eargv[eargc++] = "-";
				break;
				/* interactive display */
			case 'i': def.inter = 1;
				continue;
				/* set overlay file */
			case 'o': if(--argc > 0) def.overlay = *++argv;
				continue;
			default:
				emess(0,"invalid runline option:",arg);
				break;
			}
			break;
		}
		else /* stack input files */
			eargv[eargc++] = *argv;
	}
	if (! eargc) /* no line sources specified */
		eargv[eargc++] = "-";
	if (! def.overlay)
		def.inter = 1;
	if (def.overlay && defopen(def.overlay)) /* open overlay file */
		emess(2,"overlay file",def.overlay);
		/* process line files */
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			emess(-2,"input file:",*eargv);
			continue;
		}
		dogrid(fid);
	}

	draftit();

	/* clear out pen(s) */
	clrpen();

	if (def.overlay)	/* close defered file */
		defclose();

	plotend();	/* interactive plotter */

	exit(0);

}


# define MAX_LINE 150
# define MAXC 30

static char *s, *argv[MAXC], line[MAX_LINE];
	char
*getline();

# define LLEFT 1
# define LRIGHT 2

struct LAXIS {
	double	del;
	char	format[30];
	int	label;
	int	ticks;
} *lp;

static struct LAXIS lx = {0., "%g", LLEFT, LLEFT+LRIGHT};
static struct LAXIS ly = {0., "%g", LLEFT, LLEFT+LRIGHT};

static setup = 1;

dogrid(fid) int *fid; {
	int control(), scalein();

	while ((s = getline(line, MAX_LINE, fid)) && setup) {
		if (*s == '#') ++s;
		setup = words(s, MAXC, argv, scalein);
	}

	for ( ; s ; s = getline(line, MAX_LINE, fid)) {
		if (*s == '#') ++s;
		words(s, MAXC, argv, control);
	}
}
	static
startpen() {
	plotopt(CBASE);
	plotopt(NEWPEN, "P");
	plotopt(WXL, 0L);
	plotopt(WXH, def.x.board);
	plotopt(WYL, 0L);
	plotopt(WYH, def.y.board);
}
init() { startpen(); }
/* remove defined pens */
clrpen() { plotopt(DELPEN); }

static float s_size;
static char s_font[30] = "-";

control(argc, argv) char **argv; int argc; {
	int c, v;
	double f, atof();
	char *arg;

	while (argc-- > 0) if ((c = **argv) == '-') for (arg = *argv++;;) {
		switch (*++arg) {
		case '\0': break;
		case 's':	/* size of lettering */
			if (argc-- > 0)
				s_size = atof(*argv++);
			continue;
		case 'p':	/* select mechanical pen */
			if (argc-- > 0)
				plotopt(MPEN, (long)atoi(*argv++));
			continue;
		case 'f':	/* font */
			if (argc-- > 0)
				strcpy(s_font, *argv++);
			continue;
		case 'x':
		case 'y':
			c = *arg;
			continue;
		case 'd':	/* basic intervals */
			if (c == '-') continue;
			if (argc-- > 0) {
				f = atof(*argv++);
				if (c == 'x')	lx.del = f;
				else		ly.del = f;
			}
			continue;
		case 't':	/* ticks */
			if (c == '-') continue;
			if (argc-- > 0) {
				v = strtol(*argv++, 0, 0);
				if (c == 'x')	lx.ticks = v;
				else		ly.ticks = v;
			}
			continue;
		case 'l':	/* labels */
			if (c == '-') continue;
			if (argc-- > 0) {
				v = strtol(*argv++, 0, 0);
				if (c == 'x')	lx.label = v;
				else		ly.label = v;
			}
			continue;
		case 'F':	/* format */
			if (c == '-') continue;
			if (argc-- > 0) {
				if (c == 'x')	strcpy(lx.format, *argv++);
				else		strcpy(ly.format, *argv++);
			}
			continue;
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
}

static struct AXIS *up;
static int mode;

	static
tick(v, t, size, k) long v, size; char *t; {
	long x, y;
	int tog;

	tog = k & LLEFT;
	if (mode == 'x') {
		x = v;
		y = tog ? up->min:up->max;
		if (lp->ticks & k)
			pxyxmit(_PENUP, x, y + (tog?size:-size));
	} else {
		y = v;
		x = tog? up->min:up->max;
		if (lp->ticks & k)
			pxyxmit(_PENUP, x + (tog?size:-size), y);
	}
	pxyxmit(lp->ticks & k ? 0:_PENUP, x, y);
	if (t)
		plotopt(TEXT, t);
}

static calib;

	static
linear(p, k) struct AXIS *p; {
	static double del, z;
	static int idel;
	static int i, is, ie;
	long v, size;
	char s[20], *t;

	if (calib) {
		double r, e, min, max;

		calib = 0;
		if (lp->del != 0.) {
			z = fabs(lp->del * def.cts / p->scale);
			e = z / (r = pow(10., floor(log10(z)))); 
			if (e <= 1.5)		{ idel = 10;	del = 1. * r;
			} else if (e <= 3.5)	{ idel = 5;	del = 2. * r;
			} else if (e <= 7.5)	{ idel = 2;	del = 5. * r;
			} else			{ idel = 10;	del = 10. * r; }
		} else if (p->del != 0.) {
			idel = p->sub;
			if (idel <= 0) idel = 1;
			del = p->del / idel;
		} else
			return(0);
		if (p->dmin > p->dmax) {
			min = p->dmax;
			max = p->dmin;
		} else {
			min = p->dmin;
			max = p->dmax;
		}
		is = floor(min / del - .0001) + 1;
		ie = ceil(max / del + .0001);
	}
	for ( i = is; i != ie ; ++i) {
		z = i * del;
		v = z * p->scale + p->offset;
		if (!(i % idel)) { /* major tick */
			if (lp->label & k) {
				sprintf(s, lp->format, z);
				for (t = s; *t == ' '; ++t) ;
			} else
				t = 0;
			size = s_size;
		} else {
			size = .5 * s_size;
			t = 0;
		}
		tick(v, t, size, k);
	}
	return (1);
}
static struct LOGT {
	int flag;
	float off;
} logtick[] = {
	0x14, 0.041393,  /* 1.100000 */
	0x16, 0.079181,  /* 1.200000 */
	0x14, 0.113943,  /* 1.300000 */
	0x16, 0.146128,  /* 1.400000 */
	0x15, 0.176091,  /* 1.500000 */
	0x16, 0.204120,  /* 1.600000 */
	0x14, 0.230449,  /* 1.700000 */
	0x16, 0.255273,  /* 1.800000 */
	0x14, 0.278754,  /* 1.900000 */
	0xff, 0.301030,  /* 2.000000 */
	0x14, 0.322219,  /* 2.100000 */
	0x16, 0.342423,  /* 2.200000 */
	0x14, 0.361728,  /* 2.300000 */
	0x16, 0.380211,  /* 2.400000 */
	0x15, 0.397940,  /* 2.500000 */
	0x16, 0.414973,  /* 2.600000 */
	0x14, 0.431364,  /* 2.700000 */
	0x16, 0.447158,  /* 2.800000 */
	0x14, 0.462398,  /* 2.900000 */
	0xff, 0.477121,  /* 3.000000 */
	0x10, 0.491362,  /* 3.100000 */
	0x16, 0.505150,  /* 3.200000 */
	0x10, 0.518514,  /* 3.300000 */
	0x16, 0.531479,  /* 3.400000 */
	0x15, 0.544068,  /* 3.500000 */
	0x16, 0.556303,  /* 3.600000 */
	0x10, 0.568202,  /* 3.700000 */
	0x16, 0.579784,  /* 3.800000 */
	0x10, 0.591065,  /* 3.900000 */
	0xff, 0.602060,  /* 4.000000 */
	0x10, 0.612784,  /* 4.100000 */
	0x16, 0.623249,  /* 4.200000 */
	0x10, 0.633468,  /* 4.300000 */
	0x16, 0.643453,  /* 4.400000 */
	0x11, 0.653213,  /* 4.500000 */
	0x16, 0.662758,  /* 4.600000 */
	0x10, 0.672098,  /* 4.700000 */
	0x16, 0.681241,  /* 4.800000 */
	0x10, 0.690196,  /* 4.900000 */
	0xff, 0.698970,  /* 5.000000 */
	0x10, 0.707570,  /* 5.100000 */
	0x14, 0.716003,  /* 5.200000 */
	0x10, 0.724276,  /* 5.300000 */
	0x14, 0.732394,  /* 5.400000 */
	0x12, 0.740363,  /* 5.500000 */
	0x14, 0.748188,  /* 5.600000 */
	0x10, 0.755875,  /* 5.700000 */
	0x14, 0.763428,  /* 5.800000 */
	0x10, 0.770852,  /* 5.900000 */
	0xff, 0.778151,  /* 6.000000 */
	0x10, 0.785330,  /* 6.100000 */
	0x10, 0.792392,  /* 6.200000 */
	0x10, 0.799341,  /* 6.300000 */
	0x10, 0.806180,  /* 6.400000 */
	0x16, 0.812913,  /* 6.500000 */
	0x10, 0.819544,  /* 6.600000 */
	0x10, 0.826075,  /* 6.700000 */
	0x10, 0.832509,  /* 6.800000 */
	0x10, 0.838849,  /* 6.900000 */
	0xff, 0.845098,  /* 7.000000 */
	0x10, 0.851258,  /* 7.100000 */
	0x10, 0.857332,  /* 7.200000 */
	0x10, 0.863323,  /* 7.300000 */
	0x10, 0.869232,  /* 7.400000 */
	0x16, 0.875061,  /* 7.500000 */
	0x10, 0.880814,  /* 7.600000 */
	0x10, 0.886491,  /* 7.700000 */
	0x10, 0.892095,  /* 7.800000 */
	0x10, 0.897627,  /* 7.900000 */
	0xff, 0.903090,  /* 8.000000 */
	0x10, 0.908485,  /* 8.100000 */
	0x10, 0.913814,  /* 8.200000 */
	0x10, 0.919078,  /* 8.300000 */
	0x10, 0.924279,  /* 8.400000 */
	0x16, 0.929419,  /* 8.500000 */
	0x10, 0.934498,  /* 8.600000 */
	0x10, 0.939519,  /* 8.700000 */
	0x10, 0.944483,  /* 8.800000 */
	0x10, 0.949390,  /* 8.900000 */
	0xff, 0.954243,  /* 9.000000 */
	0x10, 0.959041,  /* 9.100000 */
	0x10, 0.963788,  /* 9.200000 */
	0x10, 0.968483,  /* 9.300000 */
	0x10, 0.973128,  /* 9.400000 */
	0x16, 0.977724,  /* 9.500000 */
	0x10, 0.982271,  /* 9.600000 */
	0x10, 0.986772,  /* 9.700000 */
	0x10, 0.991226,  /* 9.800000 */
	0x10, 0.995635,  /* 9.900000 */
	0,	0. };
	static
loga(p, k) struct AXIS *p; {
	char s[20], *t;
	struct LOGT *l;
	long v, size;
	static int mask;
	static double vm, vms, vme, vv;

	if (calib) {
		calib = 0;
		mask = (fabs((double)(p->wmax - p->wmin)) /
			fabs((double)(p->dmax - p->dmin))) + .5;
		if (mask < 3.)
			mask = 0;
		else if (mask < 5.)
			mask = 1;
		else if (mask < 10.)
			mask = 2;
		else if (mask < 20.)
			mask = 4;
		else
			mask = 0xff;
		vms = ceil((double)p->dmin);
		vme = floor((double)p->dmax);
	}
	for (vm = vms - 1.; vm <= vme; ++vm) {
		if (vm >= p->dmin) {
			v = vm * p->scale + p->offset;
			if (lp->label & k) {
				sprintf(s, lp->format, pow(10.,vm));
				for (t = s; *t == ' '; ++t) ;
			}  else
				t = 0;
			size = s_size;
			tick(v, t, size, k);
		}
		t = 0;
		for (l = logtick; l->flag; ++l) {
			if ((vv = vm + l->off) > p->dmax)
				break;
			else if (vv < p->dmin)
				continue;
			if (l->flag & 0x80)
				size = .7 * s_size;
			else if (l->flag & mask)
				size = .4 * s_size;
			else
				continue;
			v = vv * p->scale + p->offset;
			tick(v, t, size, k);
		}
	}
	return(1);
}
draftit() {
	int (*ll)();

	s_size *= def.cts;
	plotopt(SFONT, s_font);
	plotopt(SIZE, fcharsz(s_size / 21.));
	calib = 1;
	lp = &lx;
	plotopt(CENTER);
	mode = 'x';
	up = &def.y;
	ll = def.x.log ? loga : linear;
	plotopt(YOFF, (long)(-1.3 * s_size));
	if ((*ll)(&def.x, LLEFT)) {
		plotopt(YOFF, (long)(1.3 * s_size));
		(*ll)(&def.x, LRIGHT);
	}
	calib = 1;
	lp = &ly;
	plotopt(YOFF, 0L);
	mode = 'y';
	up = &def.x;
	ll = def.y.log ? loga : linear;
	plotopt(XOFF, (long)(-s_size));
	plotopt(JRIGHT);
	if ((*ll)(&def.y, LLEFT)) {
		plotopt(JLEFT);
		plotopt(XOFF, (long)s_size);
		(*ll)(&def.y, LRIGHT);
	}
}
