#ifndef lint
static char *SCCSID = "@(#)geod.c	USGS v.1.2:0";
#endif
/* <<<< Geodesic filter program >>>> */
# include <stdio.h>
# include <ctype.h>
# include <string.h>
# include "geodesic.h"
# include <math.h>

# define MAXLINE 200
# define TAB putchar('\t')
# define TWOPI		6.2831853071795864769
	extern double
atof(), dmstor(), strtod();
	/* following 4 lines for emess usage */
	extern char
*File_name, *Prog_name;
	extern int
File_line;
typedef	struct { double u, v;} UV;
	extern char
*rtodms();	/* DMS output formatter */
	static int
fullout = 0,	/* output full set of geodesic values */
tag = '#',	/* beginning of line tag character */
pos_azi = 0,	/* output azimuths as positive values */
inverse = 0;	/* != 0 then inverse geodesic */
	static char
*oform = (char *)0,	/* output format for decimal degrees */
*osform = "%.3f",	/* output format for S */
pline[50],		/* work string */
*usage =
"usage(v. 1.0): %s [ -acfFptTwW [args] ] [ +opts[=arg] ] [ files ]\n";
	static void
printLL(p, l) double p, l; {
	if (oform) {
		printf(oform, p); TAB;
		printf(oform, l);
	} else {
		fputs(rtodms(pline, p, 'N', 'S'),stdout); TAB;
		fputs(rtodms(pline, l, 'E', 'W'),stdout);
	}
}
	static void
do_arc() {
	double az, adjlon();

	printLL(phi2, lam2); putchar('\n');
	for (az = al12; n_alpha--; ) {
		al12 = az = adjlon(az + del_alpha);
		geod_prefor();
		geod_forwd();
		printLL(phi2, lam2); putchar('\n');
	}
}
	static void	/* generate intermediate geodesic coordinates */
do_geod() {
	double phil, laml, del_S;

	phil = phi2;
	laml = lam2;
	printLL(phi1, lam1); putchar('\n');
	for ( S = del_S = S / n_S; --n_S; S += del_S) {
		geod_forwd();
		printLL(phi2, lam2); putchar('\n');
	}
	printLL(phil, laml); putchar('\n');
}
	void static	/* file processing function */
process(fid) FILE *fid; {
	char line[MAXLINE+3], *s;

	for (;;) {
		++File_line;
		if (!(s = fgets(line, MAXLINE, fid)))
			break;
		if (!strchr(s, '\n')) { /* overlong line */
			int c;
			strcat(s, "\n");
			/* gobble up to newline */
			while ((c = fgetc(fid)) != EOF && c != '\n') ;
		}
		if (*s == tag) {
			fputs(line, stdout);
			continue;
		}
		phi1 = dmstor(s, &s);
		lam1 = dmstor(s, &s);
		if (inverse) {
			phi2 = dmstor(s, &s);
			lam2 = dmstor(s, &s);
			geod_invrs();
		} else {
			al12 = dmstor(s, &s);
			S = strtod(s, &s);
			geod_prefor();
			geod_forwd();
		}
		if (!*s && (s > line)) --s; /* assumed we gobbled \n */
		if (pos_azi) {
			if (al12 < 0.) al12 += TWOPI;
			if (al21 < 0.) al21 += TWOPI;
		}
		if (fullout) {
			printLL(phi1, lam1); TAB;
			printLL(phi2, lam2); TAB;
			if (oform) {
				printf(oform, al12); TAB;
				printf(oform, al21); TAB;
				printf(osform, S);
			}  else {
				fputs(rtodms(pline, al12, 0, 0), stdout);TAB;
				fputs(rtodms(pline, al21, 0, 0), stdout);TAB;
				printf(osform, S);
			}
		} else if (inverse)
			if (oform) {
				printf(oform, al12); TAB;
				printf(oform, al21); TAB;
				printf(osform, S);
			} else {
				fputs(rtodms(pline, al12, 0, 0), stdout); TAB;
				fputs(rtodms(pline, al21, 0, 0), stdout); TAB;
				printf(osform, S);
			}
		else {
			printLL(phi2, lam2); TAB;
			if (oform)
				printf(oform, al21);
			else
				fputs(rtodms(pline, al21, 0, 0), stdout);
		}
		fputs(s, stdout);
	}
}
	void
main(argc, argv) char **argv; {
	char *arg, **eargv = argv, *strnchr();
	FILE *fid;
	static int eargc = 0, c;

	if (Prog_name = strrchr(*argv,'/')) ++Prog_name;
	else Prog_name = *argv;
	inverse = ! strncmp(Prog_name, "inv", 3);
		/* process run line arguments */
	while (--argc > 0) { /* collect run line arguments */
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0': /* position of "stdin" */
				if (arg[-1] == '-') eargv[eargc++] = "-";
				break;
			case 'a': /* output full set of values */
				fullout = 1;
				continue;
			case 't': /* set col. one char */
				if (arg[1]) tag = *++arg;
				else emess(1,"missing -t col. 1 tag");
				continue;
			case 'c': /* alternate source of control */
				if (--argc <= 0)
noargument:			   emess(1,"missing argument for -%c",*arg);
				file_opt(*++argv);
				continue;
			case 'W': /* specify seconds precision */
			case 'w': /* -W for constant field width */
				if ((c = arg[1]) && isdigit(c)) {
					set_rtodms(c - '0', *arg == 'W');
					++arg;
				} else
				    emess(1,"-W argument missing or non-digit");
				continue;
			case 'f': /* alternate output format degrees or xy */
				if (--argc <= 0) goto noargument;
				oform = *++argv;
				continue;
			case 'F': /* alternate output format degrees or xy */
				if (--argc <= 0) goto noargument;
				osform = *++argv;
				continue;
			case 'p': /* output azimuths as positive */
				pos_azi = 1;
				continue;
			default:
				fprintf(stderr,usage,Prog_name);
				emess(1, "invalid option: -%c",*arg);
				break;
			}
			break;
		} else if (**argv == '+') /* + argument */
			load_opt(*argv, 0);
		else /* assumed to be input file name(s) */
			eargv[eargc++] = *argv;
	}
	/* done with parameter and control input */
	c = *(int *)param("binv","");
	if (!inverse) /* check for oldstyle and alternate inverse option */
		inverse = c;
	else if (!c)
		load_opt("+inv", 0);
	geod_setup(inverse); /* setup projection */
	if ((n_alpha || n_S) && eargc)
		emess(1,"files specified for arc/geodesic mode");
	if (n_alpha)
		do_arc();
	else if (n_S)
		do_geod();
	else { /* process input file list */
		if (eargc == 0) /* if no specific files force sysin */
			eargv[eargc++] = "-";
		for ( ; eargc-- ; ++eargv) {
			if (**eargv == '-') {
				fid = stdin;
				File_name = "<stdin>";
			} else {
				if ((fid = fopen(*eargv, "r")) == NULL) {
					emess(-2, *eargv, "input file");
					continue;
				}
				File_name = *eargv;
			}
			File_line = 0;
			process(fid);
			fclose(fid);
			File_name = (char *)0;
		}
	}
	exit(0); /* normal completion */
}
