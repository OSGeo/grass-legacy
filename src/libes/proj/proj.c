/* <<<< Cartographic projection filter program >>>> */
#ifndef lint
static char RCSID[] = "@(#)$Id: proj.c,v 4.5 1992/07/14 01:42:39 gie Exp $";
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include "projects.h"
/*
#include "emess.h"
*/

#define MAX_LINE 200
#define MAX_PARGS 100
	static PJ *Proj;
	static UV (*proj) PROTO ((UV, PJ *));
	static int
reversein = 0,	/* != 0 reverse input arguments */
reverseout = 0,	/* != 0 reverse output arguments */
bin_in = 0,	/* != 0 then binary input */
bin_out = 0,	/* != 0 then binary output */
echoin = 0,	/* echo input data to output line */
tag = '#',	/* beginning of line tag character */
inverse = 0,	/* != 0 then inverse projection */
prescale = 0,	/* != 0 apply cartesian scale factor */
postscale = 0;

	static char
*cheby_str,		/* string controlling Chebychev evaluation */
*oform = (char *)0,	/* output format for x-y or decimal degrees */
*oterr = "*\t*",	/* output line for unprojectable input */
#ifdef __STDC__
*usage = "usage($Revision: 4.5 $):"
	" %s [ -bceEfiIlormsStTwW [args] ] [ +opts[=arg] ] [ files ]\n";
#else
*usage = "usage($Revision: 4.5 $): %s [ -bceEfiIlormsStTwW [args] ] [ +opts[=arg] ] [ files ]\n";
#endif
	static struct FACTORS sfactors = {0.,0.,0.,0.,0.,0.,1e-7};
	static double
(*informat)PROTO((const char *, char **)), /* input data deformatter function */
fscale = 0.;	/* cartesian scale factor */

static UV
int_proj (data) UV data; 
{
	if (prescale) { data.u *= fscale; data.v *= fscale; }
	data = (*proj)(data, Proj);
	if (postscale && data.u != HUGE_VAL)
		{ data.u *= fscale; data.v *= fscale; }
	return(data);
}
	static void	/* file processing function */
#ifdef __STDC__
process (FILE *fid) 
#else 
process (fid) 
    FILE *fid;
#endif

{
	char line[MAX_LINE+3], *s, pline[40];
	UV data;

	for (;;) {
		++emess_dat.File_line;
		if (bin_in) {	/* binary input */
			if (fread(&data, sizeof(UV), 1, fid) != 1)
				break;
		} else {	/* ascii input */
			if (!(s = fgets(line, MAX_LINE, fid)))
				break;
			if (!strchr(s, '\n')) { /* overlong line */
				int c;
				(void)strcat(s, "\n");
				/* gobble up to newline */
				while ((c = fgetc(fid)) != EOF && c != '\n') ;
			}
			if (*s == tag) {
				if (!bin_out)
					(void)fputs(line, stdout);
				continue;
			}
			if (reversein) {
				data.v = (*informat)(s, &s);
				data.u = (*informat)(s, &s);
			} else {
				data.u = (*informat)(s, &s);
				data.v = (*informat)(s, &s);
			}
			if (data.v == HUGE_VAL)
				data.u = HUGE_VAL;
			if (!*s && (s > line)) --s; /* assumed we gobbled \n */
			if (!bin_out && echoin) {
				int t;
				t = *s;
				*s = '\0';
				(void)fputs(line, stdout);
				*s = t;
				putchar('\t');
			}
		}
		if (data.u != HUGE_VAL) {
			if (prescale) { data.u *= fscale; data.v *= fscale; }
			data = (*proj)(data, Proj);
			if (postscale && data.u != HUGE_VAL)
				{ data.u *= fscale; data.v *= fscale; }
		}
		if (bin_out) { /* binary output */
			(void)fwrite(&data, sizeof(UV), 1, stdout);
			continue;
		} else if (data.u == HUGE_VAL) /* error output */
			(void)fputs(oterr, stdout);
		else if (inverse && !oform) {	/*ascii DMS output */
			if (reverseout) {
				(void)fputs(rtodms(pline, data.v, 'N', 'S'), stdout);
				putchar('\t');
				(void)fputs(rtodms(pline, data.u, 'E', 'W'), stdout);
			} else {
				(void)fputs(rtodms(pline, data.u, 'E', 'W'), stdout);
				putchar('\t');
				(void)fputs(rtodms(pline, data.v, 'N', 'S'), stdout);
			}
		} else {	/* x-y or decimal degree ascii output */
			if (inverse) {
				data.v *= RAD_TO_DEG;
				data.u *= RAD_TO_DEG;
			}
			if (reverseout) {
				(void)printf(oform,data.v); putchar('\t');
				(void)printf(oform,data.u);
			} else {
				(void)printf(oform,data.u); putchar('\t');
				(void)printf(oform,data.v);
			}
		}
		if (pj_sfactors) /* print scale factor data */
			if (sfactors.h != HUGE_VAL)
				(void)printf("\t<%g %g %g %g %g %g>",
					sfactors.h, sfactors.k, sfactors.s,
					sfactors.omega_2 * RAD_TO_DEG, sfactors.a, sfactors.b);
			else
				(void)fputs("\t<* * * * * *>", stdout);
		(void)fputs(bin_in ? "\n" : s, stdout);
	}
}

static char *pargv[MAX_PARGS];
static int pargc = 0;

static void /* load parameters from a file */
#ifdef __STDC__
file_opt(char *name)
#else
file_opt(name)
    char *name;
#endif
{
	FILE *fid;
	char option[MAX_LINE];
	int c;
	char *s;

	emess_dat.File_name = name;
	emess_dat.File_line = 0;
	if (!(fid = fopen(name, "r"))) {
		emess(2,"parameter option file");
	}
	while (fscanf(fid,"%s",option) == 1) {
		++emess_dat.File_line;
		if (*option == '#') { /* check for and flush comments */
			while((c = fgetc(fid)) >= 0 && c != '\n') ;
			if (c < 0)
				break;
			else
				continue;
		}
		s = option;
		while (s = strtok(s, " \t\n")) {
			if (*s == '#')
				break;
			if (*s == '+')
				++s;
			if (c = strlen(s)) {
				if (pargc >= MAX_PARGS)
					emess(1,"option input overflowed option table");
				if (!(pargv[pargc] = malloc(c+1)))
					emess(2,"option input mem failure");
				(void)strcpy(pargv[pargc++], s);
			}
			s = 0;
		}
	}
	(void)fclose(fid);
}
	void
#ifdef __STDC__
main(int argc, char **argv) 
#else
main(argc, argv) 
int argc;
char **argv;
#endif
{
	char *arg, **eargv = argv, *pargv[MAX_PARGS], **iargv = argv;
	FILE *fid;
	int iargc = argc, eargc = 0, c, mon = 0;

	if (emess_dat.Prog_name = strrchr(*argv,'/')) ++emess_dat.Prog_name;
	else emess_dat.Prog_name = *argv;
	inverse = ! strncmp(emess_dat.Prog_name, "inv", 3);
	if (argc <= 1 ) {
		(void)fprintf(stderr, usage, emess_dat.Prog_name);
		exit (0);
	}
		/* process run line arguments */
	while (--argc > 0) { /* collect run line arguments */
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0': /* position of "stdin" */
				if (arg[-1] == '-') eargv[eargc++] = "-";
				break;
			case 'b': /* binary I/O */
				bin_in = bin_out = 1;
				continue;
			case 'v': /* monitor dump of initialization */
				mon = 1;
				continue;
			case 'i': /* input binary */
				bin_in = 1;
				continue;
			case 'o': /* output binary */
				bin_out = 1;
				continue;
			case 'I': /* alt. method to spec inverse */
				inverse = 1;
				continue;
			case 'E': /* echo ascii input to ascii output */
				echoin = 1;
				continue;
			case 'S': /* compute scale factors */
				pj_sfactors = &sfactors;
				continue;
			case 't': /* set col. one char */
				if (arg[1]) tag = *++arg;
				else emess(1,"missing -t col. 1 tag");
				continue;
			case 'l': /* list projections or ellipses */
				if (!arg[1] || arg[1] == 'p') {
					struct PJ_LIST *lp;
					for (lp = pj_list ; lp->id ; ++lp)
						(void)printf("%9s %s\n", lp->id, lp->name);
				} else if (arg[1] == 'e') {
					struct PJ_ELLPS *le;

					for (le = pj_ellps; le->id ; ++le)
						(void)printf("%9s %-16s %-16s %s\n",
							le->id, le->major, le->ell, le->name);
				} else
					emess(1,"invalid list option: l%c",arg[1]);
				emess(1,"-l[p|e] terminates program");
				continue; /* artificial */
			case 'e': /* error line alternative */
				if (--argc <= 0)
noargument:			   emess(1,"missing argument for -%c",*arg);
				oterr = *++argv;
				continue;
			case 'c': /* alternate source of control */
				if (--argc <= 0) goto noargument;
				file_opt(*++argv);
				continue;
			case 'T': /* generate Chebyshev coefficients */
				if (--argc <= 0) goto noargument;
				cheby_str = *++argv;
				continue;
			case 'm': /* cartesian multiplier */
				if (--argc <= 0) goto noargument;
				postscale = 1;
				if (!strncmp("1/",*++argv,2) || 
				    !strncmp("1:",*argv,2)) {
					if((fscale = atof((*argv)+2)) == 0.)
						goto badscale;
					fscale = 1. / fscale;
				} else
					if ((fscale = atof(*argv)) == 0.) {
badscale:
					      emess(1,"invalid scale argument");
					}
				continue;
			case 'W': /* specify seconds precision */
			case 'w': /* -W for constant field width */
				if ((c = arg[1]) == 0 && isdigit(c)) {
					set_rtodms(c - '0', *arg == 'W');
					++arg;
				} else
				    emess(1,"-W argument missing or non-digit");
				continue;
			case 'f': /* alternate output format degrees or xy */
				if (--argc <= 0) goto noargument;
				oform = *++argv;
				continue;
			case 'r': /* reverse input */
				reversein = 1;
				continue;
			case 's': /* reverse output */
				reverseout = 1;
				continue;
			default:
				emess(1, "invalid option: -%c",*arg);
				break;
			}
			break;
		} else if (**argv == '+') { /* + argument */
			if (pargc < MAX_PARGS)
				pargv[pargc++] = *argv + 1;
			else
				emess(1,"overflowed + argument table");
		} else /* assumed to be input file name(s) */
			eargv[eargc++] = *argv;
	}
	if (eargc == 0 && !cheby_str) /* if no specific files force sysin */
		eargv[eargc++] = "-";
	else if (eargc > 0 && cheby_str) /* warning */
		emess(4, "data files when generating Chebychev prohibited");
	/* done with parameter and control input */
	if (inverse && postscale) {
		prescale = 1;
		postscale = 0;
		fscale = 1./fscale;
	}
	if (!(Proj = pj_init(pargc, pargv)))
		emess(3,"projection initialization failure");
	if (inverse) {
		if (!Proj->inv)
			emess(3,"inverse projection not available");
		proj = pj_inv;
	} else
		proj = pj_fwd;
	if (cheby_str)
		exit(gen_cheby(inverse, int_proj, cheby_str, Proj, iargc, iargv));
	/* set input formating control */
	if (mon)
		pj_pr_list(Proj);
	if (inverse)
		informat = strtod;
	else {
		informat = dmstor;
		if (!oform)
			oform = "%.2f";
	}
	/* process input file list */
	for ( ; eargc-- ; ++eargv) {
		if (**eargv == '-') {
			fid = stdin;
			emess_dat.File_name = "<stdin>";
		} else {
			if ((fid = fopen(*eargv, "r")) == NULL) {
				emess(-2, *eargv, "input file");
				continue;
			}
			emess_dat.File_name = *eargv;
		}
		emess_dat.File_line = 0;
		process(fid);
		(void)fclose(fid);
		emess_dat.File_name = 0;
	}
	exit(0); /* normal completion */
}
