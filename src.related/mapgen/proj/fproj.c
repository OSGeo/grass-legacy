#ifndef lint
static char *SCCSID = "@(#)fproj.c	USGS v.3.4:0";
#endif
/* <<<< Cartographic projection filter using Chebyshev >>>> */
# include <stdio.h>
# include <ctype.h>
# include <string.h>
# include <math.h>

# define MAXLINE 200
# define RAD_TO_DEG	57.29577951308232
	double
dmstor(), strtod();
	/* following 4 lines for emess usage */
	extern char
*File_name, *Prog_name;
	extern int
File_line;
typedef	struct { double u, v;} UV;
	extern char
*rtodms();	/* DMS output formatter */
	extern UV
bcheval();	/* Chebyshev projection function */
	static int
reversein = 0,	/* != 0 reverse input arguments */
reverseout = 0,	/* != 0 reverse output arguments */
bin_in = 0,	/* != 0 then binary input */
bin_out = 0,	/* != 0 then binary output */
tag = '#',	/* beginning of line tag character */
no_cheby = 1,	/* Chebyshev coefficients not yet read in */
inverse = 0;	/* != 0 then inverse projection */
	static char
*oform = (char *)0,	/* output format for x-y or decimal degrees */
*oterr = "*\t*",	/* output line for unprojectable input */
*cheby_file = (char *)0,/* Chebyshev input coefficient file */
*usage =
"usage(v. 3.0): %s [ -befiorstTwW [args] ] ] [ files ]\n";
	static double
(*informat)();	/* input data deformatter function */
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
		if (data.u != HUGE)
			data = bcheval(data);
		if (bin_out) { /* binary output */
			fwrite(&data, sizeof(UV), 1, stdout);
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
	void
main(argc, argv) char **argv; {
	char *arg, **eargv = argv, *strnchr();
	FILE *fid;
	static int eargc = 0, c;

	if (Prog_name = strrchr(*argv,'/')) ++Prog_name;
	else Prog_name = *argv;
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
			case 'T': /* input Chebyshev from special file */
				if (--argc <= 0) goto noargument;
				cheby_file = *++argv;
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
		} else /* assumed to be input file name(s) */
			eargv[eargc++] = *argv;
	}
	if (eargc == 0) /* if no specific files force sysin */
		eargv[eargc++] = "-";
	/* done with parameter and control input */
	/* process Chebyshev coefficient file if given */
	if (cheby_file) {
		if ((fid = fopen(cheby_file, "r")) == NULL)
			emess(2, "failure to open coefficient file");
		if (inverse = bchload(fid)) {
			informat = strtod;
		} else {
			informat = dmstor;
			if (!oform)
				oform = "%.2f";
		}
		no_cheby = 0;
		fclose(fid);
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
		if (no_cheby) {
			if (inverse = bchload(fid)) {
				informat = strtod;
			} else {
				informat = dmstor;
				if (!oform)
					oform = "%.2f";
			}
			no_cheby = 0;
		}
		File_line = 0;
		process(fid);
		fclose(fid);
		File_name = (char *)0;
	}
	exit(0); /* normal completion */
}
