#ifndef lint
static char *SCCSID = "@(#)plegend.c	AMG v.3.1";
#endif
/* generate legend overlay */
# include <stdio.h>
# include <ctype.h>
# include <math.h>
# include <string.h>
# include "graphics.h"
# include "plotgen.h"

struct PLTDEF def;
long strtol(), mpen = 0;

main(argc, argv) char **argv; {
	char	*arg, **eargv, *strchr(), *getenv();
	int	eargc;
	FILE	*fid;

	eargv = argv;
	eargc = 0;
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
				fprintf(stderr,
					"invalid option %c\n", *arg);
				break;
			}
			break;
		} else
			eargv[eargc++] = *argv;
	}
	if (! def.overlay)
		def.inter = 1;
		/* generate legends */
	if (eargc == 0)
		eargv[eargc++] = "-";
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			emess(-2,"input file:",*eargv);
			continue;
		}
		dolegend(fid);
	}
	/* clear out pen(s) */
	clrpen();
	if (def.overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}

/* dolegend input processor */

static long xp, yp, ypo, lead;

# define MAX_LINE 200
# define MAXC 30
# define DOTEXT 1
# define DOLINE 2

static long x_min, x_max, y_min, y_max;
static char *argv[MAXC];
static char *s, line[MAX_LINE];
char *getline();
char *lincon;
	static long
xyconv(s, axis) char *s; {
	long z, low, high;
	char c;

	if (axis == 'x') {
		low = x_min;
		high = x_max;
	} else {
		low = y_min;
		high = y_max;
	}
	if ((c = *s) == '>' || c == '|')
		++s;

	z = *s ? atof(s) * def.cts : 0;
	
	if (c == '>')
		z += high;
	else if (c == '|')
		z += (high + low) / 2;
	else
		z += low;
	return (z);
}
	static
setup = 1;
dolegend(fid) int *fid; {
	int text, control(), draw(), scalein();
	long yl;

	while ((s = getline(line, MAX_LINE, fid)) && setup) {
		if (*s == '#') ++s;
		setup = words(s, MAXC, argv, scalein);
	}

	for (text = 0; s ; s = getline(line, MAX_LINE, fid))
		if (text) {
			if (*s == '.') {
				if (text == DOLINE)
					undraw();
				text = 0;
				continue;
			} else if (text == DOTEXT) {
				escline(s);
				plotopt(TEXT, s);
				yl -= lead;
				plotopt(YOFF, yl + ypo);
			} else
				words(s, MAXC, argv, draw);
		} else if (text = words(s, MAXC, argv, control)) {
			if (text == DOTEXT) {
				yl = 0;
				plotopt(YOFF, ypo);
				moveto(xp, yp);
			} else
				setdraw();
		}
}
init() {
	plotopt(CBASE);
	plotopt(NEWPEN, "L");
	plotopt(WXH, x_max = def.x.board);
	plotopt(WXL, x_min = 0);
	plotopt(WYH, y_max = def.y.board);
	plotopt(WYL, y_min = 0);
}
/* remove defined pens */
clrpen() {
	plotopt(DELPEN);
}
	static		/* insert troff text */
dotroff(type, file, pen) char *file; long pen; {
	char s[200];
	int c;
	FILE *fid, *popen();

	sprintf(s,type=='E'?
		"eqn %s |troff -TGRAPH | plroff %ld" :
		"troff -TGRAPH %s | plroff %ld", file,pen);
	if (fid = popen(s, "r")) {
		moveto(xp, yp);
		while ((c = getc(fid)) != EOF)
			plotout(c);
		pclose(fid);
	} else
		perror("plroff failure");
}
# define S_ARG 10
	static
control(argc, argv) char **argv; int argc; {
	int text, c, v, sbar();
	long f;
	double atof();
	char *arg, *s;

	text = 0;
	while (argc-- > 0)
	if ((c = **argv) == '-') for (arg = *argv++;;) {
		switch (v = *++arg) {
		case '\0':
			break;
		case 'S':	/* do symbol work */
			if (c == '-')
				c = 'S';
			continue;
		case 'c':	/* symbol to be posted */
			if (c == 'S' && argc-- > 0) {
				s = *argv++;
				if (isdigit(*s))
					plotopt(SYM,
						(int)strtol(s, (char *)0, 0));
				else
					plotopt(SYM, *s);
				moveto(xp, yp);
				plotopt(SYM, 0);
			}
			continue;
		case 't':	/* text input */
			text = DOTEXT;
			continue;
		case 'E':
		case 'T':	/* troff text */
			if (argc-- <= 0)
				continue;
			dotroff(v, *argv++, mpen);
			continue;
		case 's':	/* character size */
			if (argc-- <= 0)
				continue;
			plotopt(c == '-' ? SIZE : SSIZE,
				fcharsz(atof(*argv++) * def.cts / 21.));
			continue;
		case 'p':	/* select mechanical pen */
			if (argc-- > 0)
				plotopt(MPEN, mpen = strtol(*argv++,
					(char *)0, 0));
			continue;
		case 'a':	/* set alternate font */
		case 'f':	/* font selection */
			if (argc-- > 0)
				plotopt(c == '-' ? (v == 'f' ? SFONT:SFONTA)
					 : SFONTS, *argv++);
			continue;
		case 'r':	/* select rotation */
			if (argc-- > 0)
				plotopt(c == '-' ? ANG : SANG,
				  (long)(atof(*argv++) * DEG_TO_R * 10000));
			continue;
		case 'o':	/* offset options */
			c = 'o';
			continue;
		case 'x':	/* coordinates */
			if (argc-- > 0)
				if (c == 'o')
					plotopt(XOFF,
					   (long)(atof(*argv++) * def.cts));
				else
					xp = xyconv(*argv++, 'x');
			continue;
		case 'y':
			if (argc-- > 0)
				if (c == 'o')
					ypo = atof(*argv++) * def.cts;
				else
					yp = xyconv(*argv++, 'y');
			continue;
		case 'w':	/* select reference window */
			if (argc-- > 0)
				switch (**argv++) {
				case 'p':
					x_min = y_min = 0;
					x_max = def.x.board;
					y_max = def.y.board;
					continue;
				case 'd':
					x_min = def.x.min;
					x_max = def.x.max;
					y_min = def.y.min;
					y_max = def.y.max;
					continue;
				}
			continue;
		case 'j':	/* justification */
			if (argc-- > 0 && strchr("crl",f = **argv++))
				plotopt(f == 'c' ? CENTER : (
					f == 'r' ? JRIGHT : JLEFT));
			continue;
		case 'l':
			if (argc-- > 0) /* leading */
				lead = atof(*argv++) * def.cts;
			continue;
		case 'B':	/* Bezier line drafting */
			plotopt(BEZIER);
			goto lconon;
		case 'L':	/* line drafting */
			plotopt(BEZIERN);
lconon:
			if (argc-- > 0) {
				text = DOLINE;
				lincon = *argv++;
			}
			continue;
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
	return (text);
}
	static int
lfirst;
	static
setdraw() {
	long opt[5];
	int i;
	char *s;

	plotopt(NEWPEN, "LN:L");
	for (i = 0, s = strtok(lincon, ","); s && i < 5;
		s = strtok((char *)0, ","), i++)
		switch (i) {
		case 0: case 1:
			opt[i] = strtol(s, (char *)0, 0);
			break;
		case 2: case 3:
			opt[i] = atof(s) * def.cts;
			break;
		case 4:
			opt[i] = fcharsz(atof(s) * def.cts / 21.);
			break;
		}
	plotopt(MPEN, (int)opt[0]);
	if (i > 1) {
		if (opt[1] > 0) {
			plotopt(DMASK, (long)opt[1]);
			plotopt(DSIZE, (long)opt[2]);
			plotopt(DASH);
		}
		if (i == 5 && s) {
			plotopt(F_DIST, (long)opt[3]);
			plotopt(F_SIZE, opt[4]);
			plotopt(FSYMS, s);
			plotopt(opt[1] ? FPLOT : FPLOTN);
		}
	}
	lfirst = _PENUP;
}
	static
undraw() {
	plotopt(DELPEN);
}
	static 
draw(argc, argv) char **argv; {
	long x, y;

	if (argc--) x = xyconv(*argv++, 'x');
	if (argc) y = xyconv(*argv, 'y');
	pxyxmit(lfirst, x, y);
	lfirst = 0;
}
