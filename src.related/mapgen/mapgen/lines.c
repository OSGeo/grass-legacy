#ifndef lint
static char *SCCSID = "@(#)lines.c	AMG v.3.3";
#endif
/* generate line overlay */
# include <stdio.h>
# include <string.h>
# include "graphics.h"
# include "mapgen.h"

char	*overlay,	/* overlay file name */
	*master,	/* map master file name */
	*cntrl;	/* string of run line control */

int	inter;		/* if set, do terminal graphics */

struct map_def def;

extern int draft();

static control(), plot_it();

main(argc, argv) char **argv; {
	char	*arg, **eargv = argv;
	int	eargc = 0;
	FILE	*fid;

	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if (arg[-1] == '-')
					eargv[eargc++] = "-";
				break;
			/* interactive display */
			case 'i': inter = 1;
				continue;
			/* set overlay file */
			case 'o': if(--argc > 0) overlay = *++argv;
				continue;
			/* map master file */
			case 'm': if (--argc > 0) master = *++argv;
				continue;
			/* line control data */
			case 'c': if (--argc > 0) cntrl = *++argv;
				continue;
			default:
				fprintf(stderr,
					"invalid option %c\n", *arg);
				break;
			}
			break;
		}
		else
			eargv[eargc++] = *argv;
	}
	if (! master)
		if (eargc) { /* first file */
			--eargc;
			master = *eargv++;
		} else emess(1,"no map master given",(char *)0);
	if (! eargc) /* no line sources specified */
		eargv[eargc++] = "-";
	if (! overlay) inter = 1;
	if (loaddef(master)) /* load map master */
		exit(1);
	geoinit(0, draft, 0);
	if (inter) /* set interactive graphics */
		if (setpltr(1))
			exit(1);
	if (overlay && defopen(overlay)) /* open overlay file */
		emess(2,"overlay open failure",(char *)0);
	plotopt(CBASE);
	init();
	/* process line files */
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			perror(*eargv);
			continue;
		}
		doline(fid);
		fclose(fid);
	}
	/* clear out pen(s) */
	clrpen();
	if (overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}

# define MAX_LINE 150		/* maximum input line length		*/
# define MAXC	50		/* maximum control args / line		*/
# define MAX_FLDS	20	/* maximum posting data fields		*/
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
	int no_post;		/* number of fields			*/
	int sym;		/* point symbol				*/
};

struct lns {			/* basic control data for points	*/
	struct lns *last;	/* last point				*/
	struct consts lc;
	long x, y;		/* coordinates				*/
	long dx, dy;		/* del. current to last			*/
	int inside;		/* pt. inside lat/lon window		*/
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
	static
endseg() {
	if (last->inside) plot_it(last);
	last->inside = lastxy = oldline = 0;
}
	static
draw(argc) {
	char *s, *setfield();
	double x, y, dmstor();
	int i;

	if (argc < 2) return;	/* no coordinates */

	if (!(s = setfield(0))) return;
	x = dmstor(s, 0); resetf();
	if (!(s = setfield(1))) return;
	y = dmstor(s, 0); resetf();

	pts->oline[0] = '\0';
	for (i = 2; i < argc; i++) {
		strcat(pts->oline, setfield(i));
		strcat(pts->oline, "\n");
		resetf();
	}
	if (! oldline) { newline(x, y); oldline = 1; }
	else		conline(x, y);
	last = pts;
	pts = pts->last;
	pts->lc = last->lc;
}
doline(fid) int *fid; {
	while (s = getline(line, MAX_LINE, fid))
		if (*s == con_chr)
			words(s+1, MAXC, argv, control);
		else
			groups(s, delim, draw);
}
/* initialize point pen */
	static
startpen() {
	plotopt(NEWPEN, "P");
	plotopt(WXL, def.D.x_min);
	plotopt(WXH, def.D.x_max);
	plotopt(WYL, def.D.y_min);
	plotopt(WYH, def.D.y_max);

	pt1.last = pts = &pt0;
	pt0.last = last = &pt1;
	pt0.lc.mode = LIN_MODE;

}
init() {
	startpen();
	setform("2,1", 0);
	if (cntrl) words(cntrl, MAXC, argv, control);
}
clrpen() { /* remove defined pens */
	endseg();
	plotopt(DELPEN);
}
draft(pen, x, y) long x, y; {
	struct lns *p;
	extern int edge0, edge1;

	if (pen) {
		lastxy = 0;
		p = last;
		p->inside = (edge0 ? 0 : INSIDE) |
			(p->lc.mode & LIN_MODE ? PEN_UP : 0);
	} else {
		p = pts;
		p->inside = edge1 ? 0 : INSIDE;
	}
	p->x = x;
	p->y = y;
	if (! pen) {
		plot_it(p);
		if (edge1 && (p->lc.mode & LIN_MODE)) {
			if (p->lc.mode & SYM_MODE) plotopt(SYM, 0);
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
	if ((c = *(arg = *argv++)) == '-') for (;;) {
		switch (*++arg) {
		case '\0':
			break;
		case 'a':	/* fancy line string */
			if (argc-- > 0)
				plotopt(FSYMS, *argv++);
			continue;
		case 'b':	/* end line segment */
			endseg();
			continue;
		case 'd':
			if (c == '-') {	/* lon - lat fields */
				if (argc-- > 0)
					if (setform(*argv++, 0) != 2)
					emess(1,"must be two -d fields",
						(char *)0);
			} else if (c == 'l') /* turn dash mode on */
				plotopt(DASH);
			else if (c == 'k' && argc-- > 0) {
				/* set fancy line spacing */
					f = atof(*argv++) * def.cts_cm;
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
			fl = atof(*argv++) * def.cts_cm;
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
			if (c == '-' && argc-- > 0)
				plotopt(MPEN, (long)atoi(*argv++));
			continue;
		case 'q':	/* discontinue mode */
			switch (c) {
			case 's': pts->lc.mode &= ~ SYM_MODE; continue;
			case 'c': pts->lc.mode &= ~ CHR_MODE; continue;
			case 'l':
				pts->lc.mode &= ~ LIN_MODE;
				plotopt(BEZIERN);
				continue;
			}
			continue;
		case 'f':
			if (argc-- <= 0) continue;
			switch (c) {
			case '-': pts->lc.no_post = setform(*argv++, 2); break;
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
					(long)(atof(*argv++) * def.cts_cm));
			continue;
		case 'j':	/* jsutification */
			if (c == 'c' && argc-- > 0 &&
			   strchr("crl",f = **argv++))
				plotopt(f == 'c' ? CENTER : (
					f == 'r' ? JRIGHT : JLEFT));
			continue;
		case 'k':
			if (c == '-') /* select line options */
				c = *arg;
			else if (c == 'k')
				plotopt(FPLOTN);
			continue;
		case 'R': /* rhumb line mode */
			if ((v = strtol(++arg, &arg, 0)) > 0) {
				extern mdraw();
				geoinit(1, mdraw, 0);
			} else
				geoinit(0, draft, 0);
			rhumb(v);
			continue;
		case 'B':
			plotopt(BEZIER);
			goto lineon;
		case 'l':
			switch (c) {
			case '-': /* select line options */
lineon:
				if (arg[1] == '\0') /* turn on */
					pts->lc.mode |= LIN_MODE;
				else
					c = *arg;
				break;
			case 'l': plotopt(SOLID); break;
			case 'k': plotopt(FPLOT); break;
			case 'c': /* leading */
				if (argc-- > 0)
					plotopt(LEAD, (long)atoi(*argv++));
				break;
			}
			continue;
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
}
