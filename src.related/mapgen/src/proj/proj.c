static char *SCCSID = "@(#)proj.c	AMG v.1.2";
/*
**	Cartographic projection filter program
**
**	Developed and coded by:
**	   Gerald Ian Evenden
**	   U. S. Geological Survey
**
*/
# include <stdio.h>
# include <ctype.h>
# include <string.h>
# include "projects.h"
	double
atof();
extern struct PLIST plist[];
	/* Projection argument stack */
# define MAXLIST 40
# define MAXFIELD 2000
	char
*pargv[MAXLIST+1];
	int
pargc;
	/* general control parameters */
	static double
mult = 1.;	/* Cartesian data multiplier */
	static int
inverse = 0,	/* inverse (! 0) else forward */
orev = 0,	/* reverse output order */
binary = 0,	/* binary I/O */
tab = 0,	/* field delimeters */
lonf = 1,	/* input fields */
latf = 2,
check = 0,	/* check forward by doing inverse, for testing */
tag = '#';	/* beginning of line tag */
	static char
*proj,		/* pointer to projection structure */
*oterr = "*",	/* bad conversion tag */
*oform,		/* putput format */
*usage =
   "\nusage: proj [ -bcdefmorswt [args]] [ +args ] [ files ]";
	/* local functions */
	static void
emess(code, s, s1, s2, s3) char *s; { /* error message handler */
	char **p;

	for (p = &s; *p != 0; ++p) { /* output string(s) */
		fputs(*p, stderr);
		fputc(' ', stderr);
	}
	if (code == 2 || code == -2) /* use system on file error */
		perror();
	if (code > 0) { /* note: we bomb on positive argument */
		fputs("\nproj abnomally terminated\n", stderr);
		exit(1);
	}
	fputc('\n', stderr);
}
	static void /* load projection parameter table from file */
morecon(fid) FILE *fid; {
	register c;
	static char cwork[MAXFIELD];
	char *p;
	int n;

	p = cwork;
	n = MAXFIELD;
	for (c = getc(fid); c != EOF && pargc < MAXLIST; ++pargc) {
		for ( ; isspace(c); c = getc(fid)) ;
		if (c == '+')
			c = getc(fid);
		pargv[pargc] = p;
		for (; c != EOF && !isspace(c) && n-- > 1 ;
		   c = getc(fid))
			*p++ = c;
		if (p == pargv[pargc]) /* any found? */
			pargc--;
		else {
			n--;
			*p++ = '\0';
		}
	}
	if (pargc >= MAXLIST || n < 0)
		emess(1, "overflowed argument list area", 0);
}
	static void
setup() { /* projection initialization */
	int n;
	char *p, *malloc(), *param();
	struct PLIST *i;

		/* set forward/inverse mode */
	inverse = *(int *)param("binv");
	if (*(p = param("sproj", "")) == '\0')
		emess(1, "projection not selected",0);
		/* find projection in list */
	for (i = plist; i->name && strcmp(p, i->name); ++i) ;
	if (! i->name) /* no find */
		emess(1, p, " is invalid projection name", 0);
		/* ask projection for memory requirements */
	if ((proj = malloc((*i->projl)(0))) == NULL)
		emess(1, "memory allocation failure", 0);
		/* projection initialization */
	if (! (*i->projl)(proj, param))
		emess(1, "proj init failure",0);
	/* all failures result in termination via 'emess' 
	** a return to caller means successful initialization */
}
/******* character data handler section *******/
# define MAXLINE 200
# define MAXOUT 40
# define DEGREES 0
# define DMS 1
# define METERS 2
	static double
cartes(), (*inconv)();
	UV
*(*pr)(),	/* selected direction function */
*(*prc)();	/* revers direction for check */
	static
mode, last;
	static double
cartes(s) char *s; { return (mult * atof(s)); }
	static void
initproc() { /* processing initialization */
	double dmstor();

	if (inverse) {
		mult = 1. / mult;
		inconv = cartes;
		pr = invrs;
		if (check) /* perform inverted check */
			prc = forwd;
		if (oform) /* if formated, decimal deg. only */
			mode = DEGREES;
		else
			mode = DMS;
	} else {
		inconv = dmstor;
		pr = forwd;
		if (check) /* perform inverted check */
			prc = invrs;
		mode = METERS;
		if (!oform) /* set default format */
			oform = "%.2f";
	}
	last = --latf;
	if (last < --lonf)
		last = lonf;
}
	static void
process(fid) FILE *fid; { /* character record processor */
	int i;
	char line[MAXLINE+1], *a, *b, outa[MAXOUT], outb[MAXOUT],
		outac[10], outbc[10], *oa, *ob, *oac, *obc, *s, *p;
	UV *val, vali, *valc;

	/* record (line) loop */
	while ((s = fgets(line, MAXLINE, fid)) != NULL) {
		if (*s == tag) { /* pass through '#' records */
			fputs(line, stdout);
			continue;
		}
		/* find fields and set lat-lon source pointers */
		for (a = b = 0, i = 0; i <= last &&
		   (!tab||(tab && s)); ++i) {
			if (tab) { /* special delimiter */
				p = s;
				if ( s = strchr(s, tab) )
					*s++ = '\0';
				while (isspace(*p)) ++p;
			} else { /* white space delimiter */
				if (! (p = strtok(s, " \t\n")))
					break;
				s = 0;
			}
			/* at field, cursory check for numeric */
			if (*p && strchr("+-.0123456789", *p)) {
				/* set pointer if appropriate field */
				if (i == lonf) a = p;
				else if (i == latf) b = p;
			}
		}
		oa = ob = oac = obc = oterr;
		if ( a && b ) { /* valid fields found */
			/* deformat input */
			vali.u = (*inconv)(a, 0);
			vali.v = (*inconv)(b, 0);
			/* project */
			if (val = (*pr)(vali, proj)) {
				/* format if projection OK */
				switch (mode) {
				case DEGREES:
					sprintf(outa, oform, val->u
						* RAD_TO_DEG);
					sprintf(outb, oform, val->v
						* RAD_TO_DEG);
					break;
				case DMS:
					rtodms(outa, val->u, 'e', 'w');
					rtodms(outb, val->v, 'n', 's');
					break;
				case METERS:
					sprintf(outa, oform, mult * val->u);
					sprintf(outb, oform, mult * val->v);
					break;
				}
				oa = outa;
				ob = outb;
				if (check) { /* verification option */
					if (valc = (*prc)(*val, proj)) {
						sprintf(outac, " (%.0e)",
						   fabs(vali.u - valc->u));
						sprintf(outbc, " (%.0e)",
						   fabs(vali.v - valc->v));
						oac = outac;
						obc = outbc;
					}
				}
			}
		}
		/* output formated data */
		fputs(orev ? ob : oa, stdout);
		if (check)
			fputs(orev ? obc : oac, stdout);
		fputc('\t', stdout);
		fputs(orev ? oa : ob, stdout);
		if (check)
			fputs(orev ? oac : obc, stdout);
		fputc('\n', stdout);
	} /* end of record loop */
}
static UV error;
/***** end character data handler section *****/
	static void
binproc(in, out) { /* binary record processor */
	UV p, *r, *(*pr)();

	/* initialize */
	pr = inverse ? invrs : forwd;
	/* process loop */
	while (read(in, &p, sizeof(UV)) == sizeof(UV)) {
		if ( ! (r = (*pr)(p, proj)) )
			r = &error;
		write(out, r, sizeof(p));
	}
}
/******** system entry ********/
main(argc, argv) char **argv; {
	char *arg, **eargv;
	static char *cinput = 0, *outfil = 0;
	FILE *fid;
	static int eargc = 0;

	error.u = error.v = HUGE;
	if (argc <= 1 )
		emess(1, usage, 0);
	eargv = argv;
		/* process run line arguments */
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0': /* position of "stdin" */
				if (arg[-1] == '-') eargv[eargc++] = "-";
				break;
			case 'b': /* binary I/O */
				binary = 1; continue;
			case 'c': /* check inverse */
				check = 1; continue;
			case 'd': /* alternate delimeter characters */
				if (arg[1]) tab = *++arg;
				else emess(1,"invalid tab char",usage,0);
				continue;
			case 't': /* set col. one char */
				if (arg[1]) tag = *++arg;
				else emess(1,"invalid col. 1 tag",usage,0);
				continue;
			case 'e': /* error line alternative */
				if (--argc > 0) oterr = *++argv;
				continue;
			case 'o': /* alternate output */
				if(--argc > 0) outfil = *++argv;
				continue;
			case 'f': /* alternate source of control */
				if(--argc > 0) cinput = *++argv;
				continue;
			case 'm': /* Cartesian multiplier */
				if (--argc > 0) {
					mult = atof(*++argv);
					if (*argv = strchr(*argv,'/'))
						mult /= atof(*argv + 1);
				}
				continue;
			case 'r': /* input fields */
				lonf = strtol(++arg, &arg, 10);
				if (*arg == ',')
					latf = strtol(++arg, &arg, 10);
				else latf = lonf + 1;
				if (lonf <= 0 || lonf == latf)
					emess(1,"invalid field no.",usage,0);
				--arg;
				continue;
			case 's': /* reverse output fields */
				orev = 1; continue;
			case 'w': /* specify alternate radian output */
				if (--argc > 0) oform = *++argv;
				continue;
			default:
				emess(1, "invalid option",usage, 0);
				break;
			}
			break;
		} else if (**argv == '+')
			/* save projection arguments */
			if (pargc > MAXLIST)
				emess(1, "overflowed argument list area", 0);
			else 
				pargv[pargc++] = *argv + 1;
		else /* assumed to be input file name(s) */
			eargv[eargc++] = *argv;
	}
	if (outfil) /* re-set standard out */
		if (freopen(outfil, "w", stdout) == NULL)
			emess(2, "output:", outfil, 0);
	if (cinput) /* get additional control from file */
		if ((fid = fopen(cinput, "r")) != NULL) {
			morecon(fid);
			fclose(fid);
		} else
			emess(2, "control:", cinput, 0);
	/* done with parameter and control input */
	setup(); /* setup projection */
		/* process files */
	if (binary) /* binary mode, stdin -> stdout only */
		binproc(0, 1);
	else { /* character data */
		initproc();
		if (eargc == 0) /* if no specific files force sysin */
			eargv[eargc++] = "-";
		/* process input file list */
		for ( ; eargc-- ; ++eargv) {
			if (**eargv == '-')
				fid = stdin;
			else if ((fid = fopen(*eargv, "r")) == NULL) {
				emess(-2, "input file:", *eargv, 0);
				continue;
			}
			process(fid);
			fclose(fid);
		}
	}
	exit(0); /* normal completion */
}
