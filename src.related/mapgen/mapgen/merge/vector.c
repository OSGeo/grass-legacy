#ifndef lint
static char *SCCSID = "@(#)vector.c	OEMG v.1.1";
#endif
/* generate line overlay */
# include <stdio.h>
# include <string.h>
# include <graphics.h>
# include "gen.h"
	char	
*overlay,	/* overlay file name */
*master,	/* master file name */
*cntrl;		/* string of run line control */
	int	
con_chr ='#', /* command line character */
inter;		/* if set, do terminal graphics */
	int
draft();
	char
def_setform[40],
*getline();

# define MAX_LINE 150		/* maximum input line length		*/
# define MAXC	50		/* maximum control args / line		*/
# define MAX_FLDS	20	/* maximum posting data fields		*/
# define DEF_DEL	'\t'	/* default field delimeter		*/

int delim = DEF_DEL;	/* data field delimiter			*/

char *argvv[MAXC];
char line[MAX_LINE];
int oldline =0;
extern long strtol();
extern double strtod();
	static int
xclusion = 0;
	static void
(*base_draw)();
	static void
control(argc, argv) char **argv; int argc; {
	int c, v, opt;
	long f;
	double fl, atof();
	char *arg;

	if (xclusion) { /* first control line after exclusion turns it off */
		xclusion = 0;
		draw = base_draw;
		makewindow();
		setform(def_setform, 0);
	}
	while (argc-- > 0)
	if ((c = *(arg = *argv++)) == '-') for (;;) {
		switch (opt = *++arg) {
		case '\0':
			break;
		case 'b':	/* end line segment */
			oldline = 0;
			continue;
		case 'd':
			if (argc-- > 0) {
				strcpy(def_setform, *argv++);
				if (setform(def_setform, 0) != 2)
					emess(1,"must be two -d fields");
			} else
				emess(1,"-d fields missing");
			continue;
		case 't':	/* data field delimeter */
			if (!arg[1]) goto noiopt;
			delim = *++arg;
			continue;
		case 'z':	/* control line character */
			if (!arg[1])
noiopt:				 emess(1,"-%c immediate arg. missing",opt);
			else con_chr = *++arg;
			continue;
		case 'f':
			if (argc-- > 0)
				plotopt(SFONTS, *argv++);
			else goto noopts;
			continue;
		case 'R': /* rhumb line mode */
			/*
			if ((v = strtol(++arg, &arg, 0)) > 0) {
				extern mdraw();
				geoinit(1, mdraw, 0);
			} else
				geoinit(0, draft, 0);
			rhumb(v);
			*/
			continue;
		case 'L':
			if (argc-- > 0)
				set_line(*argv++);	
			else
noopts:				emess(1,"-%c argument[s] missing",opt);
			continue;
		case 'x': /* exclusion window data */
			setform("1,2", 0);
			oldline = 0; /* force break */
			base_draw = draw;
			draw = xwindow;
			xclusion = 1;
			continue;
		default:
			emess(1,"unknown option: -%c",opt);
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
	else
		emess(1,"unknown control information: %s", arg);
}
	static void
doline(fid) int *fid; {
	char *s;

	while (s = getline(line, MAX_LINE, fid)) {
		++File_line;
		if (*s == con_chr)
			words(s+1, MAXC, argvv, control);
		else
			groups(s, delim, draw);
	}
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
	strcpy(def_setform, data_type == PLOTGEN?"1,2":"2,1");
	setform(def_setform, 0);
	if (inter && setpltr(1)) /* set interactive graphics */
		emess(1,"failure to open program plotter");
	if (overlay && defopen(append, overlay)) /* open overlay file */
		emess(2,overlay,"overlay open failure");
	plotopt(CBASE);
	plotopt(NEWPEN, "V");
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
		doline(fid);
		fclose(fid);
	}
	/* clear out pen(s) */
	plotopt(DELPEN);
	if (overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}
