#ifndef lint
static char *SCCSID = "@(#)legend.c	OEMG v.1.3";
#endif
/* generate legend overlay */
# include <stdio.h>
# include <ctype.h>
# include <math.h>
# include <string.h>
# include <graphics.h>
# include "gen.h"
	static char
*overlay,	/* overlay file name */
*master,	/* master file name */
*cntrl;		/* string of run line control */
	static int
inter;		/* if set, do terminal graphics */
static sbar(), control();
	long
strtol(), mpen;
	/* dolegend input processor */
	static long
size, xp, yp, ypo, lead;
# define MAX_LINE 200
# define MAXC 30
# define DOTEXT 1
# define DOLINE 2
	static struct LIMITS
plot_lim = {0, 0, 0, 0},
*win = & plot_lim;
	static char
*argvv[MAXC],
*s, line[MAX_LINE],
*lincon;
	char
*getline();
	static int
lfirst;
	static
undraw() { plotopt(DELPEN); }
	static long
xyconv(s, axis) char *s; {
	long z, low, high;
	char c;

	if (axis == 'x') {
		low = win->min.x;
		high = win->max.x;
	} else {
		low = win->min.y;
		high = win->max.y;
	}
	if ((c = *s) == '>' || c == '|')
		++s;
	z = *s ? atof(s) * cts_cm : 0;
	if (c == '>')
		z += high;
	else if (c == '|')
		z += (high + low) / 2;
	else
		z += low;
	return (z);
}
	static 
ldraw(argc, argv) char **argv; {
	long x, y;

	if (argc < 2) {
		if (argc > 0)
			emess(-1,"partial line data record\n");
	} else {
		x = xyconv(*argv++, 'x');
		y = xyconv(*argv, 'y');
		pxyxmit(lfirst, x, y);
		lfirst = 0;
	}
}
dolegend(fid) int *fid; {
	int text, control();
	long yl;

	text = 0;
	while (s = getline(line, MAX_LINE, fid)) {
		++File_line;
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
				words(s, MAXC, argvv, ldraw);
		} else if (text = words(s, MAXC, argvv, control)) {
			if (text == DOTEXT) {
				yl = 0;
				plotopt(YOFF, ypo);
				moveto(xp, yp);
			} else {
				plotopt(NEWPEN, "LN:L");
				set_line(lincon);
				lfirst = _PENUP;
			}
		}
	}
}
init() {
	plotopt(NEWPEN, "L");
	plotopt(WXH, x_board);
	plotopt(WXL, 0L);
	plotopt(WYH, y_board);
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
		emess(-2,"plroff failure\n");
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
			if (c == 'S') {
				if (argc-- <= 0) goto argcount;
				s = *argv++;
				if (isdigit(*s))
					plotopt(SYM,
					   (int)strtol(s, (char *)0, 0));
				else
					plotopt(SYM, *s);
				moveto(xp, yp);
				plotopt(SYM, 0);
			} else goto unknown;
			continue;
		case 'b':	/* scale bar */
			if (data_type != MAPGEN)
				goto notmap;
			if (argc-- > 0)
				twords(*argv++, ',', S_ARG, sarg, sbar);
			else
argcount:			emess(-1,"-%c no argument given\n",v);
			continue;
		case 'd':	/* scale fraction */
			if (data_type != MAPGEN) {
notmap:
				emess(-1,"scale/bar for non-MAP not allowed");
				continue;
			}
			sprintf(str,"Scale 1:%.3f",m_def.scale);
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
			if (argc-- <= 0) goto argcount;
			dotroff(v, *argv++, mpen);
			continue;
		case 's':	/* character size */
			if (argc-- <= 0) goto argcount;
			size = (fl = atof(*argv++) * cts_cm);
			plotopt(c == '-' ? SIZE : SSIZE, fcharsz(fl/21.));
			continue;
		case 'p':	/* select mechanical pen */
			if (argc-- > 0)
				plotopt(MPEN, mpen = strtol(*argv++,
					(char *)0, 0));
			else goto argcount;
			continue;
		case 'a':	/* set alternate font */
		case 'f':	/* font selection */
			if (argc-- > 0)
				plotopt(c == '-' ? (v == 'f' ? SFONT:SFONTA)
					 : SFONTS, *argv++);
			else goto argcount;
			continue;
		case 'r':	/* select rotation */
			if (argc-- > 0)
				plotopt(c == '-' ? ANG : SANG,
				  (long)(atof(*argv++) * DEG_TO_R * 10000));
			else goto argcount;
			continue;
		case 'o':	/* offset options */
			c = 'o';
			continue;
		case 'x':	/* coordinates */
			if (argc-- > 0)
				if (c == 'o')
					plotopt(XOFF,
					   (long)(atof(*argv++) * cts_cm));
				else
					xp = xyconv(*argv++, 'x');
			else goto argcount;
			continue;
		case 'y':
			if (argc-- > 0)
				if (c == 'o')
					ypo = atof(*argv++) * cts_cm;
				else
					yp = xyconv(*argv++, 'y');
			else goto argcount;
			continue;
		case 'w':	/* select reference window */
			if (argc-- > 0)
				switch (f = **argv++) {
				case 'p': win = & plot_lim; continue;
				case 'd': win = & base_lim; continue;
				default:
					emess(-1,"-w%c invalid reference window",f);
				}
			else goto argcount;
			continue;
		case 'j':	/* justification */
			if (argc-- > 0)
				if (strchr("crl",f = **argv++))
					plotopt(f == 'c' ? CENTER : (
						f == 'r' ? JRIGHT : JLEFT));
				else
					emess(-1,"-j%c - invalid justification\n",f);
			else goto argcount;
			continue;
		case 'l':
			if (argc-- > 0) /* leading */
				lead = atof(*argv++) * cts_cm;
			else goto argcount;
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
			} else goto argcount;
			continue;
		default:
unknown:
			emess(-1,"-%c : unknown control\n", v);
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
	cms = units * bsize * 100 * cts_cm / m_def.scale;
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
	int
main(argc, argv) char **argv; {
	char	*arg, *append, **eargv = argv;
	int	eargc = 0, opt;
	FILE	*fid;

	Prog_name = *argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(opt = *++arg) {
			case '\0':
				if (arg[-1] == '-')
					eargv[eargc++] = "-";
				break;
			/* interactive display */
			case 'i': inter = 1;
				continue;
			/* set overlay file */
			case 'a':
			case 'o': if(--argc > 0) {
					overlay = *++argv;
					append = opt == 'o' ? "-w" : "-a";
				} else emess(1,"missing overlay name");
				continue;
			/* master file */
			case 'm': if (--argc > 0) master = *++argv;
				else emess(1,"missing definition name");
				continue;
			/* line control data */
			case 'c': if (--argc > 0) cntrl = *++argv;
				else emess(1,"no runline control field");
				continue;
			default:
				emess(1,"invalid control line argument:-%c",
					*arg);
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
		} else emess(1,"no definition/scaling file given");
	if (! eargc) /* no line sources specified */
		eargv[eargc++] = "-";
	if (! overlay) inter = 1;
	File_name = master;
	File_line = 0;
	pl.x = pl.y = -100;
	loader(master); /* load master */
	plot_lim.max.x = x_board;
	plot_lim.max.y = y_board;
	if (inter && setpltr(1)) /* set interactive graphics */
		emess(1,"failure to open program plotter");
	if (overlay && defopen(append, overlay)) /* open overlay file */
		emess(2,overlay,"overlay open failure");
	plotopt(CBASE);
	init();
	/* process optional runline control */
	File_name = "run line control";
	if (cntrl) words(cntrl, MAXC, argvv, control);
	/* process line files */
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-")) {
			*eargv = "<stdin>";
			fid = stdin;
		} else if ((fid = fopen(*eargv, "r")) == NULL) {
			emess(-2,eargv,"\ninput file skipped\n");
			continue;
		}
		File_name = *eargv;
		File_line = 0;
		dolegend(fid);
		fclose(fid);
	}
	/* clear out pen(s) */
	plotopt(DELPEN);
	if (overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}
