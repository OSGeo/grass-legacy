#ifndef lint
static char *SCCSID = "@(#)points.c	AMG v.3.1";
#endif
/* generate point plot overlay */

# include <string.h>
# include <stdio.h>
# include "graphics.h"
# include "mapgen.h"

char	*overlay,	/* overlay file name */
	*master,	/* map master file name */
	*cntrl;		/* string of run line control */

int	inter;		/* if set, do terminal graphics */

struct map_def def;

main(argc, argv) char **argv; {
	char	*arg;
	FILE	*fid;
	char	**eargv = argv;
	int	eargc = 0;

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
		else /* stack input files */
			eargv[eargc++] = *argv;
	}
	if (! master)
		if (eargc) { /* first file */
			--eargc;
			master = *eargv++;
		} else emess(1,"no map master given",(char *)0);
	if (! eargc) /* no line sources specified */
		eargv[eargc++] = "-";
	if (! overlay)
		inter = 1;

	if (loaddef(master)) /* load map master */
		exit(1);

	geoinit(0, 0, 0);

	if (inter) /* set interactive graphics */
		if (setpltr(1))
			exit(1);

	if (overlay && defopen(overlay)) /* open overlay file */
		emess(2,"overlay open failure",(char *)0);

	plotopt(CBASE);
	init();

	/* process point files */
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			perror(*eargv);
			continue;
		}
		dopoint(fid);
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

struct pts {			/* basic control data for points	*/
	int delim;		/* data field delimeters		*/
	int mode;		/* posting mode				*/
	int con_chr;		/* col 1 tag identifying command line	*/
};
struct pts pts = {
	DEF_DEL,
	3,
	'#',
};
	static int
sym;
control(argc, argv) char **argv; int argc; {
	int c;
	long f;
	double atof();
	char *arg;

	while (argc-- > 0)
	if ((c = *(arg = *argv++)) == '-') for (;;) {
		switch (*++arg) {
		case '\0':
			break;
		case 'd':	/* lon - lat fields */
			if (argc-- > 0)
				if (setform(*argv++, 0) < 2)
					emess(1,"both -d fields req'd",
						(char *)0);
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
			   fcharsz(atof(*argv++) * def.cts_cm / 21.));
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
			if (c == '-' && argc-- > 0)
				plotopt(MPEN, (long)atoi(*argv++));
			continue;
		case 'q':	/* discontinue mode */
			if (c == 's')
				pts.mode &= ~ SYM_MODE;
			else if (c == 'c')
				pts.mode &= ~ CHR_MODE;
			continue;
		case 'f':
			if (argc-- <= 0)
				continue;
			if (c == '-')
				setform(*argv++, 2);
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
					(long)(atof(*argv++) * def.cts_cm));
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
	static char
*argv[MAXC],
*s,
oline[MAX_LINE],
line[MAX_LINE];
	char
*getline();
	static
post(argc) {
	char *s, *setfield();
	double x, y, dmstor();
	int i;

	if (argc < 2)
		return;	/* no coordinates */

	if (!*(s = setfield(0))) return;
	x = dmstor(s, 0); resetf();
	if (!*(s = setfield(1))) return;
	y = dmstor(s, 0); resetf();

	if (! dpoint(x, y) && (argc > 2) && pts.mode & CHR_MODE) {
		oline[0] = '\0';
		for (i = 2; i < argc; i++) {
			strcat(oline, setfield(i));
			strcat(oline, "\n");
			resetf();
		}
		plotopt(TEXT, oline);
	}
}
dopoint(fid) int *fid; {
	int control();

	while (s = getline(line, MAX_LINE, fid))
		if (*s == pts.con_chr)
			words(s+1, MAXC, argv, control);
		else
			groups(s, pts.delim, post);
}
/* initialize point pen */
	static
startpen() {
	plotopt(NEWPEN, "P");
	plotopt(WXL, def.D.x_min);
	plotopt(WXH, def.D.x_max);
	plotopt(WYL, def.D.y_min);
	plotopt(WYH, def.D.y_max);
}
init() {
	startpen();
	setform("2,1", 0);
	if (cntrl)
		words(cntrl, MAXC, argv, control);
}
/* remove defined pens */
clrpen() { plotopt(DELPEN); }
