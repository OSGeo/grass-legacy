#ifndef lint
static char *SCCSID = "@(#)proj.c	USGS v.3.16:1";
#endif
/* <<<< Cartographic projection filter program >>>> */
# include <stdio.h>
# include <ctype.h>
# include <string.h>
# include <math.h>

# define MAXLINE 200
# define RAD_TO_DEG	57.29577951308232
	extern double
atof(), dmstor(), strtod();
	/* following 4 lines for emess usage */
	extern char
*File_name, *Prog_name;
	extern int
File_line;
typedef	struct { double u, v;} UV;
extern UV (*ProjSetup())();
	extern char
*rtodms();	/* DMS output formatter */
	static UV
(*proj)();	/* projection function */
	static int
reversein = 0,	/* != 0 reverse input arguments */
reverseout = 0,	/* != 0 reverse output arguments */
bin_in = 0,	/* != 0 then binary input */
bin_out = 0,	/* != 0 then binary output */
tag = '#',	/* beginning of line tag character */
inverse = 0,	/* != 0 then inverse projection */
prescale = 0,	/* != 0 apply cartesian scale factor */
postscale = 0;
	static char
*cheby_str,		/* string controlling Chebychev evaluation */
*oform = (char *)0,	/* output format for x-y or decimal degrees */
*oterr = "*\t*",	/* output line for unprojectable input */
*usage =
"usage(v. 3.1): %s [ -bcefiormstTwW [args] ] [ +opts[=arg] ] [ files ]\n";
	static double
(*informat)(),	/* input data deformatter function */
fscale = 0.;	/* cartesian scale factor */
	static UV
int_proj(data) UV data; {
	if (prescale) { data.u *= fscale; data.v *= fscale; }
	data = (*proj)(data);
	if (postscale && data.u != HUGE)
		{ data.u *= fscale; data.v *= fscale; }
	return(data);
}
	static	/* file processing function */
process(fid) FILE *fid; {
	char line[MAXLINE+3], *s, pline[40];
	UV data;

	for (;;) {
		++File_line;
		if (bin_in) {	/* binary input */
			if (fread(&data, sizeof(UV), 1, fid) != 1)
				break;
		} else {	/* ascii input */
			if (!(s = fgets(line, MAXLINE, fid)))
				break;
			if (!strchr(s, '\n')) { /* overlong line */
				int c;
				strcat(s, "\n");
				/* gobble up to newline */
				while ((c = fgetc(fid)) != EOF && c != '\n') ;
			}
			if (*s == tag) {
				if (!bin_out)
					fputs(line, stdout);
				continue;
			}
			if (reversein) {
				data.v = (*informat)(s, &s);
				data.u = (*informat)(s, &s);
			} else {
				data.u = (*informat)(s, &s);
				data.v = (*informat)(s, &s);
			}
			if (!*s && (s > line)) --s; /* assumed we gobbled \n */
		}
		if (data.u != HUGE) {
			if (prescale) { data.u *= fscale; data.v *= fscale; }
			data = (*proj)(data);
			if (postscale && data.u != HUGE)
				{ data.u *= fscale; data.v *= fscale; }
		}
		if (bin_out) { /* binary output */
			fwrite(&data, sizeof(UV), 1, stdout);
			fflush(stdout);
			continue;
		} else if (data.u == HUGE)	/* ascii error output */
			fputs(oterr, stdout);
		else if (inverse && !oform) {	/*ascii DMS output */
			if (reverseout) {
				fputs(rtodms(pline, data.v, 'N', 'S'), stdout);
				putchar('\t');
				fputs(rtodms(pline, data.u, 'E', 'W'), stdout);
			} else {
				fputs(rtodms(pline, data.u, 'E', 'W'), stdout);
				putchar('\t');
				fputs(rtodms(pline, data.v, 'N', 'S'), stdout);
			}
		} else {	/* x-y or decimal degree ascii output */
			if (inverse) {
				data.v *= RAD_TO_DEG;
				data.u *= RAD_TO_DEG;
			}
			if (reverseout) {
				printf(oform,data.v); putchar('\t');
				printf(oform,data.u);
			} else {
				printf(oform,data.u); putchar('\t');
				printf(oform,data.v);
			}
		}
		fputs(bin_in ? "\n" : s, stdout);
	}
}
	char
**iargv;
	int
iargc;
	void
main(argc, argv) char **argv; {
	char *arg, **eargv = argv, *strnchr();
	FILE *fid;
	static int eargc = 0, c;

	iargc = argc; /* save these for possible use of gen_cheby */
	iargv = argv;
	if (Prog_name = strrchr(*argv,'/')) ++Prog_name;
	else Prog_name = *argv;
	inverse = ! strncmp(Prog_name, "inv", 3);
	if (argc <= 1 ) {
		fprintf(stderr, usage, Prog_name);
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
			case 'i': /* input binary */
				bin_in = 1;
				continue;
			case 'o': /* output binary */
				bin_out = 1;
				continue;
			case 't': /* set col. one char */
				if (arg[1]) tag = *++arg;
				else emess(1,"missing -t col. 1 tag");
				continue;
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
		} else if (**argv == '+') /* + argument */
			load_opt(*argv, 0);
		else /* assumed to be input file name(s) */
			eargv[eargc++] = *argv;
	}
	if (eargc == 0 && !cheby_str) /* if no specific files force sysin */
		eargv[eargc++] = "-";
	else if (eargc > 0 && cheby_str) /* warning */
		emess(4, "data files when generating Chebychev prohibited");
	/* done with parameter and control input */
	c = *(int *)param("binv","");
	if (!inverse) /* check for oldstyle and alternate inverse option */
		inverse = c;
	else if (!c)
		load_opt("+inv", 0);
	if (inverse && postscale) {
		prescale = 1;
		postscale = 0;
		fscale = 1./fscale;
	}
	proj = ProjSetup(inverse); /* setup projection */
	if (cheby_str)
		exit(gen_cheby(inverse, int_proj, cheby_str));
	/* set input formating control */
	if (inverse) {
		informat = strtod;
	} else {
		informat = dmstor;
		if (!oform)
			oform = "%.2f";
	}
	/* process input file list */
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
	exit(0); /* normal completion */
}
