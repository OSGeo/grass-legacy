#ifndef lint
static char *SCCSID = "@(#)grid.c	AMG v.3.2";
#endif
/* generate grid overlay */

# include <stdio.h>
# include <string.h>
# include <math.h>
# include "graphics.h"
# include "mapgen.h"

char	*overlay,	/* overlay file name */
	*master;	/* map master file name */

int	inter;		/* if set, do terminal graphics */
	static
init(), postit(), ticker(), sline(), startpen(), dogrid(),
pline(), draw(), mline(), draft(), clrpen();

struct map_def def;
main(argc, argv) char **argv; {
	char	*arg, **eargv = argv;
	FILE	*fid;
	int	eargc = 0, draft();

	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0': break;

			/* interactive display */
			case 'i': inter = 1;
				continue;

			/* set overlay file */
			case 'o': if(--argc > 0) overlay = *++argv;
				continue;

			/* map master file */
			case 'm': if (--argc > 0) master = *++argv;
				continue;

			default:
				fprintf(stderr,
					"invalid option %c\n", *arg);
				break;
			}
			break;
		} else
			eargv[eargc++] = *argv;
	}
	if (! master)
		if (eargc) { /* first file */
			--eargc;
			master = *eargv++;
		} else emess(1,"no map master given",(char *)0);
	if (! overlay)
		inter = 1;
	if (loaddef(master)) /* load map master */
		exit(1);
	geoinit(1, draft, 0);
	if (inter) /* set interactive graphics */
		if (setpltr(1))
			exit(1);
	if (overlay && defopen(overlay))  /* open overlay file */
		emess(2,"overlay open failure",(char *)0);
	plotopt(CBASE);
	if (eargc == 0)
		eargv[eargc++] = "-";
	/* get control data and set up */
	while (eargc--) {
		if (! strcmp(eargv[eargc], "-"))
			fid = stdin;
		else if ((fid = fopen(eargv[eargc], "r")) == NULL) {
			perror(eargv[eargc]);
			continue;
		}
		init(fid);
	}
	/* generate grid */
	dogrid();
	/* clear out pen(s) */
	clrpen();
	if (overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}
	static struct
grids {
	double	maj_int;	/* major interval, radians		*/
	double	low_lim,	/* range limits from 'def'		*/
		upp_lim;	/*					*/
	double	maj_tik;	/* major tick size			*/
	int	min_int;	/* no. minor intervals			*/
	double	min_tik;	/* minor tick size			*/
}	mer,		/* merdians */
	par;		/* parallels */
long lshift;	/* label displacement */
long hshift;
int cpen;	/* character pen selection */
char edges[6];	/* list of character edges all= "lrbt" */

extern struct map_def def;
	static
control(argc, argv) char **argv; int argc; {
	int c;
	long strtol();
	double fabs(), atof(), dmstor();
	char *arg;
	struct grids *ptr;

	while (argc-- > 0)
	if ((c = **argv) == '-') for (arg = *argv++;;) {
		switch (*++arg) {
		case '\0':
			break;
		case 'f':	/* set font */
			if (argc-- > 0)
				plotopt(SFONTA, *argv++);
			continue;
		case 'a':	/* posting edges */
			if (argc-- > 0)
				strncpy(edges, *argv++, 5);
			continue;
		case 's':	/* set character size */
			if (argc-- > 0)
				plotopt(SIZE,
				   fcharsz(atof(*argv++) * def.cts_cm / 21.));
			continue;
		case 'b':	/* line drafting pen */
			if (argc-- > 0)
				plotopt(MPEN, (long)atoi(*argv++));
			continue;
		case 'c':	/* character pen */
			if (argc-- > 0)
				cpen = atoi(*argv++);
			continue;
		case 'd':	/* label displacement */
			if (argc-- > 0) {
				lshift = atof(*argv++) * def.cts_cm;
				hshift = .71 * lshift;
			}
			continue;
		case 'm':
			if (c == '-') { /* meridian selection */
				c = 'm';
				ptr = &mer;
			}
			continue;
		case 'p':	/* parallel */
			if (c == '-') {
				c = 'p';
				ptr = &par;
			}
			continue;
		case 'u': /* major tick size */
			if (c != '-' && argc-- > 0)
				ptr->maj_tik = atof(*argv++) * def.cts_cm;
			continue;
		case 'v': /* minor tick size */
			if (c != '-' && argc-- > 0)
				ptr->min_tik = atof(*argv++) * def.cts_cm;
			continue;
		case 'i': /* major interval */
			if (c != '-' && argc-- > 0)
				ptr->maj_int = dmstor(*argv++, 0);
			continue;
		case 'j': /* minor interval */
			if (c != '-' && argc-- > 0)
				ptr->min_int = atoi(*argv++);
			continue;
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
}
# define LABEL 1
# define TICKS 2
# define LINE 0
# define iabs(x) ((x) < 0 ? -(x) : (x))
# define MAX_LINE 150		/* maximum input line length		*/
# define MAXC	50		/* maximum control args / line		*/
extern struct srange srange;
static char *argv[MAXC];
static char line[MAX_LINE];
char *getline();
static int nodraw;
static long ticklim;
	static
init(file) FILE *file; {
	char *cntrl;

	startpen();
	while ((cntrl = getline(line, MAX_LINE, file))!=NULL)
		words(cntrl, MAXC, argv, control);
}
	static /* remove defined pens */
clrpen() { plotopt(DELPEN); }
# define minute 2.90888e-4
# define TOL 1e-12
# define TOLM 1e-7
static char label[30];
	static
draft(pen, x, y) long x, y; {
	extern edge_flag;
	static double rl, r, s, dx, dy;
	static long xl, yl, idx, idy;
	static flag;
	int kill = 0;

	if (nodraw == LABEL) { /* labeling mode */
		if (pen) {
			flag = edge_flag;
			xl = x;
			yl = y;
		} else {
			idx = x - xl;
			idy = y - yl;
			if (iabs(idx) > 10 || iabs(idy) > 10) {
				postit(xl, yl, x, y, flag);
				return(1);
			}
		}
		return(0);
	} else if (nodraw == TICKS) { /* tick control */
		if (pen) {
			xl = x;
			yl = y;
			rl = 0.;
		} else if (r = hypot(dx = x - xl, dy = y - yl)) {
			s = (ticklim - rl) / r;
			if (kill = (s <= .999999)) {
				x = xl + (long)(s * dx);
				y = yl + (long)(s * dy);
			} else {
				rl += r;
				xl = x;
				yl = y;
			}
		}
	}
	pxyxmit(pen ? _PENUP : 0, x, y);
	return(kill);
}
	extern char *
strchr();
	static
mline(p, x, y) double x, y; {
if (p) newline(x, y); else conline(x, y); }
	static
pline(p, x, y) double x, y; { if (p) newline(y, x); else conline(y, x); }
	static
draw(u, v, zline) struct grids *u, *v; int (*zline)(); {
	if ((ticklim = u->maj_tik) > 0.)
		ticker(u->low_lim, u->upp_lim, u->maj_int, 0.,
			v->low_lim, v->upp_lim, v->maj_int, zline);
	else {
		nodraw = LINE;
		sline(u->low_lim, u->upp_lim, v->low_lim, v->upp_lim,
			v->maj_int, zline, 0, 0);
	}
	if ((ticklim = u->min_tik) > 0. && u->min_int > 1)
		ticker(u->low_lim, u->upp_lim, u->maj_int,
			u->maj_int / u->min_int,
			v->low_lim, v->upp_lim, v->maj_int, zline);
}
	static
dogrid() {
	nowrap(1);
	rhumb(3);

	mer.low_lim = def.l_lon;
	mer.upp_lim = def.r_lon;
	par.low_lim = def.b_lat;
	par.upp_lim = def.t_lat;
		/* axis generation */
	draw(&mer, &par, mline);
	draw(&par, &mer, pline);
		/* labeling section */
	nodraw = LABEL;
	plotopt(MPEN, (long)cpen);
	plotopt(TEXT, "\002");	/* select alt. font */
	if (! edges[0])
		strcpy(edges, "lrtb");
	if (strchr(edges, 'l'))
		sline(mer.low_lim, mer.upp_lim, par.low_lim, par.upp_lim,
			par.maj_int, mline, 'N', 'S');
	if (strchr(edges, 'r'))
		sline(mer.upp_lim, mer.low_lim, par.low_lim, par.upp_lim,
			par.maj_int, mline, 'N', 'S');
	if (strchr(edges, 'b'))
		sline(par.low_lim, par.upp_lim, mer.low_lim, mer.upp_lim,
			mer.maj_int, pline, 'E', 'W');
	if (strchr(edges, 't'))
		sline(par.upp_lim, par.low_lim, mer.low_lim, mer.upp_lim,
			mer.maj_int, pline, 'E', 'W');
}
	static /* initialize point pen */
startpen() {
	plotopt(NEWPEN, "L");
	plotopt(SFONT, "-");
}
	static /* usable for both line and label work */
sline(x1, x2, y1, y2, y_del, zline, ps, ns)
double x1, x2, y1, y2, y_del;
int (*zline)();
{
	double yp, y, adjlon();
	char *s;

	yp = y2 + TOL;
	for (y = ceil((y1 - TOL)/y_del) * y_del; y <= yp; y += y_del) {
		if (nodraw == LABEL) {
			rtodms(label, adjlon(y), ps, ns);
			if ((s = strchr(label, 'd'))) *s = 31;
		}
		(*zline)(1, x1, y);
		(*zline)(0, x2, y);
	}
}
	static
ticker(x1, x2, x_del, x_sub, y1, y2, y_del, zline)
double x1, x2, x_del, x_sub, y1, y2, y_del;
int (*zline)();
{
	double yp, y, xp, x, t, del, tickdel;
	int mode;

	nodraw = TICKS;
	xp = x2 + TOL;
	yp = y2 + TOL;
	del = (mode = (x_sub > 0.)) ? x_sub : x_del;
	tickdel = (mode ? y_del : x_del) * .25;
	del = (mode = (x_sub > 0.)) ? x_sub : x_del;
	for (y = ceil((y1 - TOL)/y_del) * y_del; y <= yp; y += y_del)
		for (x = ceil((x1 - TOL)/del) * del; x <= xp; x += del)
			if (mode && !(((t = fmod(fabs(x), x_del)) < TOLM) ||
				(fabs(t - x_del) < TOLM))) {
				(*zline)(1, x, y);
				(*zline)(0, x, y - tickdel);
				(*zline)(1, x, y);
				(*zline)(0, x, y + tickdel);
			} else {
				(*zline)(1, x, y);
				(*zline)(0, x - tickdel, y);
				(*zline)(1, x, y);
				(*zline)(0, x + tickdel, y);
			}
}
# define EIGHT_PI .3927
	static
postit(xa, ya, xb, yb, flag) long xa, ya, xb, yb; {
	int ang;

	switch ((flag & 0xf)) {
	case LEFT:
		ang = 1;
		break;
	case RIGHT:
		ang = 3;
		break;
	case BOTTOM:
		ang = 2;
		break;
	case TOP:
		ang = 4;
		break;
	default:
		ang = 0;
		break;
	}
	ang = (ang << 4) + (
	  (int)((atan2((double)(ya - yb),(double)(xa - xb)) + PI ) / EIGHT_PI)
	  & 15);
	pxyxmit(_PENUP, xa, ya);
	switch (ang) {
	case 27: case 28: case 29: case 30:
		plotopt(YOFF, lshift); goto case0a;
	case 17: case 18: case 19: case 20:
		plotopt(YOFF, -lshift); goto case0a;
	case 0: case 15: case 16: case 31: 
		plotopt(YOFF, 0L);
case0a:		plotopt(XOFF, -lshift);
		plotopt(JRIGHT);
		break;
	case 54: case 53: case 52: case 51:
		plotopt(YOFF, -lshift); goto case7a;
	case 57: case 58: case 59: case 60:
		plotopt(YOFF, lshift); goto case7a;
	case 7: case 8: case 55: case 56:
		plotopt(YOFF, 0L);
case7a:		plotopt(XOFF, lshift);
		plotopt(JLEFT);
		break;
	case 47: case 32: case 33:
		plotopt(JRIGHT); goto case3a;
	case 38: case 39: case 40:
		plotopt(JLEFT); goto case3a;
	case 3: case 4: case 34: case 35: case 36: case 37:
		plotopt(CENTER);
case3a:		plotopt(YOFF, -lshift);
		plotopt(XOFF, 0L);
		break;
	case 78: case 79:
		plotopt(JRIGHT); goto case11a;
	case 71: case 72: case 73:
		plotopt(JLEFT); goto case11a;
	case 11: case 12: case 74: case 75: case 76: case 77:
		plotopt(CENTER);
case11a:	plotopt(YOFF, lshift);	
		plotopt(XOFF, 0L);
		break;
	case 1: case 2:
		plotopt(YOFF, -hshift);
		plotopt(XOFF, -hshift);
		plotopt(JRIGHT);
		break;
	case 5: case 6:
		plotopt(YOFF, -hshift);
		plotopt(XOFF, hshift);
		plotopt(JLEFT);
		break;
	case 9: case 10:
		plotopt(YOFF, hshift);
		plotopt(XOFF, hshift);
		plotopt(JLEFT);
		break;
	case 13: case 14:
		plotopt(YOFF, hshift);
		plotopt(XOFF, -hshift);
		plotopt(JRIGHT);
		break;
	default:
		fprintf(stderr, "postit crazy error: %d\n", ang);
	}
	plotopt(TEXT, label);
}
