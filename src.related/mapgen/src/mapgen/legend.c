#ifndef lint
static char *SCCSID = "@(#)legend.c	AMG v.3.1";
#endif
/* generate legend overlay */

# include <stdio.h>
# include <ctype.h>
# include <math.h>
# include <string.h>
# include "graphics.h"
# include "mapgen.h"

char	*overlay,	/* overlay file name */
	*master;	/* map master file name */

int	inter;		/* if set, do terminal graphics */

struct map_def def;
long strtol(), mpen;

main(argc, argv) char **argv; {
	FILE	*fid;
	char	*arg, **eargv, *getenv();
	int	eargc;

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
	if (inter) /* set interactive graphics */
		if (setpltr(1))
			exit(1);
	if (overlay && defopen(overlay)) /* open overlay file */
		emess(2,"overlay open failure",(char *)0);
	plotopt(CBASE);
	init();
	/* generate legends */
	if (eargc == 0)
		eargv[eargc++] = "-";
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			perror(*eargv);
			continue;
		}
		dolegend(fid);
	}
	/* clear out pen(s) */
	clrpen();
	if (overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}
/* dolegend input processor */

static long size, xp, yp, ypo, lead;

# define MAX_LINE 200
# define MAXC 30
# define DOTEXT 1
# define DOLINE 2

static struct WINDOW_ *win = & def.B;
static char *argv[MAXC];
static char *s, line[MAX_LINE];
char *getline();
char *lincon;

static long xyconv(s, axis) char *s; {
	long z, low, high;
	char c;

	if (axis == 'x') {
		low = win->x_min;
		high = win->x_max;
	} else {
		low = win->y_min;
		high = win->y_max;
	}
	if ((c = *s) == '>' || c == '|')
		++s;
	z = *s ? atof(s) * def.cts_cm : 0;
	if (c == '>')
		z += high;
	else if (c == '|')
		z += (high + low) / 2;
	else
		z += low;
	return (z);
}
dolegend(fid) int *fid; {
	int text, control(), draw();
	long yl;

	text = 0;
	while (s = getline(line, MAX_LINE, fid))
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
	plotopt(NEWPEN, "L");
	plotopt(WXH, def.B.x_max);
	plotopt(WXL, 0L);
	plotopt(WYH, def.B.y_max);
	plotopt(WYL, 0L);
}
	/* remove defined pens */
clrpen() { plotopt(DELPEN); }
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
	double atof(), fl;
	char *arg, str[30], *s;
	static char *sarg[S_ARG];

	text = 0;
	while (argc-- > 0)
	if ((c = **argv) == '-') for (arg = *argv++;;) {
		switch (v = *++arg) {
		case '\0':
			break;
		case 'S':	/* symbol work */
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
		case 'b':	/* scale bar */
			if (argc-- > 0)
				twords(*argv++, ',', S_ARG, sarg, sbar);
			continue;
		case 'd':	/* scale fraction */
			sprintf(str,"Scale 1:%.3f",def.scale);
			s = & str[strlen(str)-1];
			while (*s == '0') s--;
			if (*s == '.')
				s--;
			*++s = '\0';
			moveto(xp, yp);
			plotopt(YOFF, 0L);
			plotopt(TEXT, str);
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
			size = (fl = atof(*argv++) * def.cts_cm);
			plotopt(c == '-' ? SIZE : SSIZE, fcharsz(fl/21.));
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
					   (long)(atof(*argv++) * def.cts_cm));
				else
					xp = xyconv(*argv++, 'x');
			continue;
		case 'y':
			if (argc-- > 0)
				if (c == 'o')
					ypo = atof(*argv++) * def.cts_cm;
				else
					yp = xyconv(*argv++, 'y');
			continue;
		case 'w':	/* select reference window */
			if (argc-- > 0)
				switch (**argv++) {
				case 'p': win = & def.B; continue;
				case 'd': win = & def.D; continue;
				case 'A': win = & def.S1; continue;
				case 'B': win = & def.S2; continue;
				}
			continue;
		case 'j':	/* justification */
			if (argc-- > 0 && strchr("crl",f = **argv++))
				plotopt(f == 'c' ? CENTER : (
					f == 'r' ? JRIGHT : JLEFT));
			continue;
		case 'l':
			if (argc-- > 0) /* leading */
				lead = atof(*argv++) * def.cts_cm;
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
	static
sbar(narg, argv) char **argv; {
	static struct {
		char *unit;
		char *name;
		double scale;
	} list[] = {
		"ft", "FEET", .3048,
		"kf", "KILO-FEET", 304.8,
		"m",  "METERS", 1.,
		"km", "KILOMETERS", 1000.,
		"yd", "YARDS", .9144,
		"ky", "KILO-YARDS", 914.4,
		"kn", "NAUTICAL MILES", 1853.25,
		"mi", "STATUTE MILES", 1609.35,
		0, 0, 0.};
	char *s, *name, field[10];
	int i, bsize, rep, left, toggle;
	long b, t, u, m;
	double units, cms;

	name = 0;
	if (narg < 3)
		return;
	narg -= 3;
	bsize = atoi(*argv++);
	if (isdigit(*(s = *argv++)))
		units = atof(s);
	else {
		for (i = 0; list[i].unit; i++)
			if (!strcmp(list[i].unit, s))
				break;
		if (! list[i].unit)
			return;
		units = list[i].scale;
		name = list[i].name;
	}
	rep = atoi(*argv++);
	left = (narg-- > 0) ? atoi(*argv++) : 0;
	if (narg > 0) name = *argv;
	plotopt(NEWPEN, "LN:L");
	cms = units * bsize * 100 * def.cts_cm / def.scale;
	t = xp - (long)(left ? cms : 0);
	b = xp + (long)(cms * rep);
	moveto(t, yp); lineto(b, yp);
	u = yp + size;
	m = (u + yp) / 2;
	moveto(b, u); lineto(t, u);
	plotopt(XOFF, 0L); plotopt(YOFF, (long)size); plotopt(CENTER);
	plotopt(ANG, 0L);
	toggle = 1;
	for (i = left ? -1: 0; i <= rep; i++) {
		moveto(b = t, yp); lineto(t, u);
		sprintf(field,"%d",abs(bsize * i));
		plotopt(TEXT,field);
		if (left > 1) {
			long c, ct;
			int i;

			c = cms / left;
			ct = t;
			for (i = 1; i < left ; i++) {
				if (toggle) moveto(b, m);
				ct += c;
				if (toggle) lineto(ct, m);
				toggle ^= 1;
				moveto(b = ct, yp);
				lineto(ct, u);
			}
			left = 0;
		}
		if (i != rep) {
			if (toggle) moveto(b, m);
			t += cms;
			if (toggle) lineto(t, m);
			toggle ^= 1;
		}
	}
	if (name != 0) {
		plotopt(JLEFT);
		plotopt(XOFF,
		  (long)((size * (strlen(field) + 3)) / 2));
		plotopt(TEXT, name);
	}
	plotopt(DELPEN);
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
			opt[i] = atof(s) * def.cts_cm;
			break;
		case 4:
			opt[i] = fcharsz(atof(s) * def.cts_cm / 21.);
			break;
		}
	plotopt(MPEN, (long)opt[0]);
	if (i > 1) {
		if (opt[1] > 0) {
			plotopt(DMASK, (long)opt[1]);
			plotopt(DSIZE, (long)opt[2]);
			plotopt(DASH);
		}
		if (i == 5 && s) {
			plotopt(F_DIST, (long)opt[3]);
			plotopt(F_SIZE, (long)opt[4]);
			plotopt(FSYMS, s);
			plotopt(opt[1] ? FPLOT : FPLOTN);
		}
	}
	lfirst = _PENUP;
}
	static
undraw() { plotopt(DELPEN); }
	static 
draw(argc, argv) char **argv; {
	long x, y;

	if (argc--) x = xyconv(*argv++, 'x');
	if (argc) y = xyconv(*argv, 'y');
	pxyxmit(lfirst, x, y);
	lfirst = 0;
}
