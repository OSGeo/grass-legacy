#ifndef lint
static char *SCCSID = "@(#)ppoint.c	AMG v.3.2";
#endif
/* generate point plot overlay */

# include <stdio.h>
# include <string.h>
# include "graphics.h"
# include "plotgen.h"

struct PLTDEF def;

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
		dopoint(fid);
	}

	/* clear out pen(s) */
	clrpen();

	if (def.overlay)	/* close defered file */
		defclose();

	plotend();	/* interactive plotter */

	exit(0);

}

/*	point input processor */


# define MAX_LINE 150		/* maximum input line length		*/
# define MAXC	50		/* maximum control args / line		*/
# define DEF_DEL	'\t'	/* default field delimeter		*/
# define SYM_MODE	1	/* symbol mode bit			*/
# define CHR_MODE	2	/* character mode			*/

static struct {			/* basic control data for points	*/
	int delim;		/* data field delimeters		*/
	int mode;		/* posting mode				*/
	int con_chr;		/* col 1 tag identifying command line	*/
} pts = {
	DEF_DEL,
	3,
	'#',
};

static int sym;

static char *argv[MAXC];
static char *s, line[MAX_LINE];
char *getline();

static char oline[MAX_LINE];

static
post(argc)
{
	double x, y, atof(), log10();
	char *s, *setfield();
	int i;

	if (argc < 2)
		return;	/* no coordinates */
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

	if (! dpoint(x, y, 0) && pts.mode & CHR_MODE) {
		oline[0] = '\0';
		for (i = 2; i < argc; i++) {
			strcat(oline, setfield(i));
			resetf();
			strcat(oline, "\n");
		}
		plotopt(TEXT, oline);
	}
}

static setup = 1;

dopoint(fid) int *fid; {
	int control(), scalein();

	while ((s = getline(line, MAX_LINE, fid)) && setup) {
		if (*s == '#') ++s;
		setup = words(s, MAXC, argv, scalein);
	}

	for ( ; s ; s = getline(line, MAX_LINE, fid))
		if (*s == pts.con_chr)
			words(s+1, MAXC, argv, control);
		else
			groups(s, pts.delim, post);
}
/* initialize point pen */
	static
startpen() {
	plotopt(CBASE);
	plotopt(NEWPEN, "P");
	plotopt(WXL, 0L);
	plotopt(WXH, def.x.board);
	plotopt(WYL, 0L);
	plotopt(WYH, def.y.board);
}
init() { setform("1,2",0); startpen(); }
/* remove defined pens */
clrpen() { plotopt(DELPEN); }

control(argc, argv) char **argv; int argc; {
	int c;
	long f;
	double atof();
	char *arg;

	while (argc-- > 0)
	if ((c = **argv) == '-') for (arg = *argv++;;) {
		switch (*++arg) {
		case '\0':
			break;
		case 'd':	/* x - y fields */
			if (argc-- > 0)
				if (setform(*argv++, 0) != 2)
				emess(1,"invalid no. -d fields",(char *)0);
			continue;
		case 't':	/* data field delimeter */
			if (arg[1])
				pts.delim = *++arg;
			continue;
		case 'z':	/* control line character */
			if (arg[1])
				pts.con_chr = *++arg;
			continue;
		case 's':
			if (c == '-') { /* select symbol options */
				if (arg[1] == '\0') /* turn on */
					plotopt(SYM, sym);
				else
					c = 's';
				continue;
			}
			if (argc-- <= 0)
				continue;
			plotopt(c == 's' ? SSIZE : SIZE,
				fcharsz(atof(*argv++) * def.cts / 21.));
			continue;
		case 'c':
			if (c == '-')	/* select character options */
				if (arg[1] == '\0') /* turn on */
					pts.mode |= CHR_MODE;
				else
					c = 'c';
			else if (c == 's' && argc-- > 0) { /* set symbol */
				sym = **argv;
				if (strchr("0123456789", sym))
					sym = atoi(*argv);
				argv++;
				plotopt(SYM, sym);
			}
			continue;
		case 'p':	/* select mechanical pen */
			if (argc-- > 0)
				plotopt(MPEN, (long)atoi(*argv++));
			continue;
		case 'q':	/* discontinue mode */
			if (c == 's')
				plotopt(SYM, 0);
			else if (c == 'c')
				pts.mode &= ~ CHR_MODE;
			continue;
		case 'f':
			if (argc-- <= 0)
				continue;
			if (c == '-')
				setform(*argv++,2);
			else
				plotopt(c == 's' ? SFONTS : SFONT, *argv++);
			continue;
		case 'r':	/* select rotation */
			if (c != '-' && argc-- > 0)
				plotopt(c == 's' ? SANG : ANG,
				  (long)(atof(*argv++) * DEG_TO_R * 10000));
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
		case 'l':	/* leading */
			if (c == 'c' && argc-- > 0)
				plotopt(LEAD, (long)atoi(*argv++));
			continue;
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
}
