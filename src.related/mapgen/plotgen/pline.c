#ifndef lint
static char *SCCSID = "@(#)pline.c	AMG v.3.3";
#endif
/* generate line plot overlay */

# include <string.h>
# include <stdio.h>
# include "graphics.h"
# include "plotgen.h"

struct PLTDEF def;

static plot_it(), control();

main(argc, argv) char **argv; {
	char	*arg;
	FILE	*fid;
	char	**eargv;
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
		doline(fid);
		fclose(fid);
	}

	/* clear out pen(s) */
	clrpen();

	if (def.overlay)	/* close defered file */
		defclose();

	plotend();	/* interactive plotter */

	exit(0);

}

# define MAX_LINE 150		/* maximum input line length		*/
# define MAXC	50		/* maximum control args / line		*/
# define DEF_DEL	'\t'	/* default field delimeter		*/
# define SYM_MODE	1	/* symbol mode bit			*/
# define CHR_MODE	2	/* character mode			*/
# define LIN_MODE	4	/* line drafting mode			*/
# define P_OK		4
# define PEN_UP		2
# define INSIDE		1

struct consts {
	int mode;		/* posting mode				*/
	int orthos, orthoc;	/* orthogonal/parallel flags		*/
	int sym;		/* point symbol				*/
};

struct lns {			/* basic control data for points	*/
	struct lns *last;	/* last point				*/
	struct consts lc;
	long x, y;		/* coordinates				*/
	long dx, dy;		/* del. current to last			*/
	int inside;		/* pt. inside x/y window		*/
	char oline[MAX_LINE];	/* posting data				*/
};

static struct lns pt1, pt0;

struct lns *pts, *last;

int delim = DEF_DEL;		/* data field delimeter			*/
int con_chr = '#';		/* col 1 tag identifying command line	*/

static char *argv[MAXC];
static char *s, line[MAX_LINE];
char *getline();

static int oldline, lastxy;
static long lastx, lasty;

endseg() {
	if (last->inside)
		plot_it(last);
	last->inside = lastxy = oldline = 0;
}
	static
draw(argc) {
	double x, y, atof(), log10();
	char *s, *setfield();
	int i;

	if (argc < 2) return;	/* no coordinates */

	if (!*(s = setfield(0))) return;
	x = atof(s); resetf();
	if (!*(s = setfield(1))) return;
	y = atof(s); resetf();
	if (def.x.log) {
		if (x <= 0.) return;
		else x = log10(x);
	}
	if (def.y.log) {
		if (y <= 0.) return;
		else y = log10(y);
	}
	pts->oline[0] = '\0';
	for (i = 2; i < argc; i++) {
		strcat(pts->oline, setfield(i));
		strcat(pts->oline, "\n");
		resetf();
	}
	if (! oldline) {
		newline(x, y);
		oldline = 1;
	} else
		conline(x, y);
	last = pts;
	pts = pts->last;
	pts->lc = last->lc;
}

static setup = 1;

doline(fid) int *fid; {
	int control(), scalein();

	while ((s = getline(line, MAX_LINE, fid)) && setup) {
		if (*s == '#') ++s;
		setup = words(s, MAXC, argv, scalein);
	}
	for ( ; s ; s = getline(line, MAX_LINE, fid))
		if (*s == con_chr)
			words(s+1, MAXC, argv, control);
		else
			groups(s, delim, draw);
}

/* initialize point pen */

long xlo, xhi, ylo, yhi;
int c0, c1;
	static
startpen() {
	plotopt(CBASE);
	plotopt(NEWPEN, "P");
	plotopt(WXL, 0L);
	plotopt(WXH, def.x.board);
	plotopt(WYL, 0L);
	plotopt(WYH, def.y.board);

	xlo = def.x.min;
	xhi = def.x.max;
	ylo = def.y.min;
	yhi = def.y.max;

	pt1.last = pts = &pt0;
	pt0.last = last = &pt1;
	pt0.lc.mode = LIN_MODE;
}
init() {
	setform("1,2",0); /* default x-y fields */
	startpen();
}
clrpen() { /* remove defined pens */
	endseg();
	plotopt(DELPEN);
}
draft(pen, x, y) long x, y; {
	struct lns *p;

	if (pen) {
		lastxy = 0;
		p = last;
		p->inside = (c0 ? 0 : INSIDE) |
			(p->lc.mode & LIN_MODE ? PEN_UP : 0);
	} else {
		p = pts;
		p->inside = c1 ? 0 : INSIDE;
	}
	p->x = x;
	p->y = y;
	if (! pen) {
		plot_it(p);
		if (c1 && (p->lc.mode & LIN_MODE)) {
			if (p->lc.mode & SYM_MODE)
				plotopt(SYM, 0);
			pxyxmit(0, x, y);
			last->inside = lastxy = 0;
		}
	}
}
	static long
adjang(a, type) long a; {
	if (type == 'o')	a += a < 0 ? 15708 : -15708;
	else if (a > 15708)	a -= 31416;
	else if (a < -15708)	a += 31416;
	return (a);
}
	static
plot_it(p) struct lns *p; {
	static int symlast;
	double atan2();
	long ang;

	if (last->inside & INSIDE) {
		if (last->lc.orthoc || last->lc.orthos) {
			ang = atan2((double)(p->y - (lastxy?lasty:last->y)),
				(double)(p->x - (lastxy?lastx:last->x))) *
				10000.;
			if (last->lc.orthoc)
				plotopt(ANG, adjang(ang, last->lc.orthoc));
			if (last->lc.orthos)
				plotopt(SANG, adjang(ang, last->lc.orthos));
		}
		if (last->lc.mode & SYM_MODE) {
			if (symlast != last->lc.sym)
				plotopt(SYM, symlast = last->lc.sym);
		} else if (symlast)
			plotopt(SYM, symlast = 0);
	} else if (symlast)
		plotopt(SYM, symlast = 0);

	pxyxmit((!(last->lc.mode & LIN_MODE) || last->inside & PEN_UP) ?
		_PENUP : 0, last->x, last->y);

	if ((last->inside & INSIDE) && (last->lc.mode & CHR_MODE))
		plotopt(TEXT, last->oline);

	lastxy = 1;
	lastx = last->x;
	lasty = last->y;
}
	static
control(argc, argv) char **argv; int argc; {
	int c, v;
	long f, strtol();
	double fl, atof();
	char *arg;

	while (argc-- > 0)
	if ((c = **argv) == '-') for (arg = *argv++;;) {
		switch (*++arg) {
		case '\0':
			break;
		case 'a':	/* symbol line string */
			if (argc-- > 0)
				plotopt(FSYMS, *argv++);
			continue;
		case 'b':	/* end line segment */
			endseg();
			continue;
		case 'd':
			if (c == '-') {	/* x - y fields */
				if (argc-- > 0)
					if (setform(*argv++, 0)!= 2)
					emess(1,"invalid no. -d fields",
						(char *)0);
			} else if (c == 'l') /* turn dash mode on */
				plotopt(DASH);
			else if (c == 'k' && argc-- > 0) {
				/* set symbol line spacing */
				f = atof(*argv++) * def.cts;
				plotopt(F_DIST, f);
			}
			continue;
		case 't':	/* data field delimeter */
			if (arg[1])
				delim = *++arg;
			continue;
		case 'z':	/* control line character */
			if (arg[1])
				con_chr = *++arg;
			continue;
		case 's':
			if (c == '-') { /* select symbol options */
				if (arg[1] == '\0') /* turn on */
					pts->lc.mode |= SYM_MODE;
				else
					c = 's';
				continue;
			}
			if (argc-- <= 0)
				continue;
			fl = atof(*argv++) * def.cts;
			switch (c) {
				case 's': v = SSIZE; break;
				case 'c': v =  SIZE; break;
				case 'k': v = F_SIZE; break;
				default: plotopt(DSIZE, (long)fl); continue;
			}
			plotopt(v, fcharsz(fl / 21.));
			continue;
		case 'm':	/* dash line mask */
			if (c == 'l' && argc-- > 0) {
				f = strtol(*argv++, (char *)0, 0);
				plotopt(DMASK, f);
			}
			continue;
		case 'c':
			if (c == '-')	/* select character options */
				if (arg[1] == '\0') /* turn on */
					pts->lc.mode |= CHR_MODE;
				else
					c = 'c';
			else if (c == 's' && argc-- > 0) { /* set symbol */
				pts->lc.sym = **argv;
				if (strchr("0123456789", pts->lc.sym))
					pts->lc.sym = atoi(*argv);
				argv++;
			}
			continue;
		case 'p':	/* select mechanical pen */
			if (argc-- > 0)
				plotopt(MPEN, (long)atoi(*argv++));
			continue;
		case 'q':	/* discontinue mode */
			switch (c) {
			case 's': pts->lc.mode &= ~ SYM_MODE; break;
			case 'c': pts->lc.mode &= ~ CHR_MODE; break;
			case 'l':
				pts->lc.mode &= ~ LIN_MODE;
				plotopt(BEZIERN);
				break;
			}
			continue;
		case 'f':
			if (argc-- > 0)
				switch (c) {
				case '-':
				if (c == '-') setform(*argv++, 2); break;
				case 'c': plotopt(SFONT, *argv++); break;
				case 's': plotopt(SFONTS, *argv++); break;
				}
			continue;
		case 'r':	/* select rotation */
			if ((c == 's' || c == 'c') && argc-- > 0) {
				if ((v = **argv) == 'o' || v == 'p')
					argv++;
				else {
					plotopt(c == 's' ? SANG : ANG,
				  (long)(atof(*argv++) * DEG_TO_R * 10000));
					v = 0;
				}
				if (c == 'c') pts->lc.orthoc = v;
				else pts->lc.orthos = v;
			}
			continue;
		case 'x':	/* offsets */
		case 'y':
			if (c == 'c' && argc-- > 0)
				plotopt(*arg == 'x' ? XOFF : YOFF ,
					(long)(atof(*argv++) * def.cts));
			continue;
		case 'j':	/* jsutification */
			if (c == 'c' && argc-- > 0 &&
					strchr("crl",f = **argv++))
				plotopt(f == 'c' ? CENTER : (
					f == 'r' ? JRIGHT : JLEFT));
			continue;
		case 'k':
			if (c == '-') c = *arg;
			else if (c == 'k') plotopt(FPLOTN);
			continue;
		case 'B':
			plotopt(BEZIER);
			goto lineon;
		case 'l':
			switch(c) {
			case '-': /* select line options */
lineon:
				if (arg[1] == '\0') /* turn on */
					pts->lc.mode |= LIN_MODE;
				else
					c = 'l';
				break;
			case 'l': plotopt(SOLID); break;
			case 'k': plotopt(FPLOT); break;
			case 'c':
				if (argc-- > 0) /* leading */
					plotopt(LEAD, (long)atoi(*argv++));
				break;
			}
			continue;
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
}
