#ifndef lint
static char *SCCSID = "@(#)plotter.c	USGS v.4.3";
#endif
#include <signal.h>
#include <varargs.h>
#define PLOTTER
#define MAIN_PROG
#include "graphics.h"
#include "plotter.h"
extern void exit();
extern long strtol();
extern char *strcpy();
	extern struct DEV_LIST
dev_list[];
	BASE
base = { 0, 0, NULL, };
	long
p_base_x, p_base_y,
base_x, base_y;
	char
*font_dir[] = {"\0", "\0"};
	static char
*env_name = _GENVB,
*env_base,
*initmap,
*ofile;	/* output file name */

PEN *pen = 0;	/* pen control block pointer */

static void pmapin(), initincl();

int error = 0;	/* error status */
	unsigned
pmaplist[] = { /* mechanical pen mapping array */
	  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
	 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
	 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
	 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
	 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
	 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
	 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
	 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
	 80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
	 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
	100,101,102,103,104,105,106,107,108,109,
	110,111,112,113,114,115,116,117,118,119,
	120,121,122,123,124,125,126,127,128,129,
	130,131,132,133,134,135,136,137,138,139,
	140,141,142,143,144,145,146,147,148,149,
	150,151,152,153,154,155,156,157,158,159,
	160,161,162,163,164,165,166,167,168,169,
	170,171,172,173,174,175,176,177,178,179,
	180,181,182,183,184,185,186,187,188,189,
	190,191,192,193,194,195,196,197,198,199,
	200,201,202,203,204,205,206,207,208,209,
	210,211,212,213,214,215,216,217,218,219,
	220,221,222,223,224,225,226,227,228,229,
	230,231,232,233,234,235,236,237,238,239,
	240,241,242,243,244,245,246,247,248,249,
	250,251,252,253,254,255
};

XY *(*device)();
	static void
onintr() { /* interrupt trap */
	if (device) (*device)(D_PANIC);
	exit (1);
}
	void
bomb(va_alist) va_dcl {
	va_list vargs;
	char *fmt;
	int opt;
	extern void perror();

	va_start(vargs);
	opt = va_arg(vargs, int);
	(void)fputs("plotter failure, Ver.4.0\n",stderr);
	if (opt) perror("SYSTEM error:");
	fmt = va_arg(vargs, char *);
	vfprintf(stderr, fmt, vargs);
	va_end(vargs);
	onintr();
}
	static char * /* word splitter for env data */
nextenv(code) {
	char *s, *strchr();

	if (env_base) {
		s = env_base;
		if (env_base = strchr(env_base, code))
			*env_base++ = '\0';
	} else
		s = "\0";
	return s;
}
	static void
setdev(name) char *name; {
	int i;
	char *getenv(), *s;
		/* if name not given, use terminal's */
	if (! name)
		if (!(name = getenv("GTERM")))
			name = getenv("TERM");
		/* look up name in list */
	for (i = 0; s = dev_list[i].name ; i++)
		if (!strcmp(s, name)) {
			device = dev_list[i].dev;
			Dglobal.model_no = dev_list[i].model;
			break;
		}
	/* couldn't find a legit name. use last in list! */
	if (! s)
		device = dev_list[i-1].dev;
	else if (!ofile) /* hard on-line device */
		while (*(s = nextenv(','))) {
			if (!strcmp(dev_list[i].name, s)) {
				ofile = nextenv(':');
				break;
			}
			(void)nextenv(':');
		}
	if (ofile && !freopen(ofile,"w+",stdout))
		bomb(1, "output file (-o): %s\n",ofile);
}
main(argc, argv) int argc; char *argv[]; {
	double atof();
	long strtol();
	XY *p;
	int i;
	char *pstr, *devname = (char *)0;
	char **aargv, *arg, *getenv(), *strchr();
	int aargc;

	/* set interupt trap */
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void)signal(SIGINT, onintr);
	if (!(env_base = getenv(env_name)))
		bomb(0,"environment: %s, not found\n", env_name);
	/* set default font directory and font pointer */
	(void)nextenv(':'); /* skip over program's name */
	init_pens();
	for (i = 0; i < 2; ++i)
		font_dir[i] = nextenv(':');
	aargv = argv;
	aargc = 0;
	while (--argc > 0) { /* process options */
		if (**++argv == '-') for (arg = *argv;;) {
			switch (*++arg) {
			case '\0': /* stdin file */
				if (arg[-1] == '-')
					aargv[aargc++] = "-";
				break;
			case 'o': /* output file */
				if (--argc > 0)
					ofile = *++argv;
				else
noarg:					bomb(0,"no arg. for -%c\n",*arg);
				continue;
			case 'X': /* x base offset */
				if (--argc > 0)
					p_base_x = base_x = strtol(*++argv,0,0);
				else goto noarg;
				continue;
			case 'Y': /* y base offset */
				if (--argc > 0)
					p_base_y = base_y = strtol(*++argv,0,0);
				else goto noarg;
				continue;
			case 's': /* scale factor */
				if (--argc > 0) Dglobal.scale = atof(*++argv);
				else goto noarg;
				continue;
			case 'd': /* device name */
				if (--argc > 0) devname = *++argv;
				else goto noarg;
				continue;
			case 'D': /* device arg */
				if (Dglobal.dargc <  MAX_DARGS)
					Dglobal.dargv[Dglobal.dargc++] = ++arg;
				else
					bomb(0,"Overflowed %d -D args\n",
						MAX_DARGS);
				break;
			case 'r': /* reverse axis */
				Dglobal.reverse = 1;
				continue;
			case 'q': /* mute <bell prompt> */
				Dglobal.quiet = 1;
				continue;
			case 'i': /* interactive */
				if (--argc > 0) {
					pstr = "-               ";
					(void)strcpy(pstr+1,*++argv);
					aargv[aargc++] = pstr;
					continue;
				} else
					bomb(0,"no interactive link args\n");
				continue;
			case 'x': /* x offset */
			case 'y': /* y offset */
			case 'p': /* pen mapping */
				aargv[aargc++] = *argv;
				break;
			case 'P': /* initial pen mapping */
				initmap = *argv + 2;
				break;
			default: /* invalid argument */
				bomb(0, "invalid option: -%c\n", *arg);
			}
			break;
		} else /* input file */
			aargv[aargc++] = *argv;
	}
	if (initmap) pmapin(initmap);
	if (! aargc ) aargv[aargc++] = "-";
	setdev(devname);
	if ((p = (*device)(D_INIT)) == NULL)
		bomb(0,"device initialization failure: %s\n",devname);
	windinit((long)(p->x), (long)(p->y));
	while ( aargc-- ) {
		arg = *aargv++;
		if (*arg == '-')
			switch (arg[1]) {
			case 'p':
				pmapin( arg + 2 );
				continue;
			case 'x':
				base_x = p_base_x +
					strtol(arg+2,(char **)0,10);
				continue;
			case 'y':
				base_y = p_base_y +
					strtol(arg+2,(char **)0,10);
				continue;
			}
		initincl(arg);
		pltparse();
	}
	(*device)(D_DONE);
	exit(0);
#ifdef lint
return 1;
#endif
}
#define MAXINCLUDE 5
#define MAXSTRING  256

static file_no, direct;
static FILE *ilist[MAXINCLUDE], *answr;
static long xb[MAXINCLUDE], yb[MAXINCLUDE];

/* initialize include pre-processor */
	static void
initincl(file) char *file; {
	char *strchr();

	if (direct = (*file == '-' && file[1] != '\0')) {
		if ((ilist[0] = fdopen(atoi(++file),"r")) == NULL)
		      bomb(1, "pipe input open failure\n");
		file = strchr(file, '.');
		if (!file || (answr = fdopen(atoi(++file),"w")) == NULL)
		      bomb(1, "pipe output open failure\n");
	}
	else if (*file == '-')
		ilist[0] = stdin;
	else if ((ilist[0] = fopen(file,"r")) == NULL)
		bomb(1, "input file: %s\n",file);
	file_no = 0;
	base.x = base_x;
	base.y = base_y;
}
metain() { /* get next meta-graphic byte */
	static c;

	while (file_no >= 0 && (c = fgetc(ilist[file_no])) == EOF) {
		base.x = xb[file_no];
		base.y = yb[file_no];
		(void)fclose(ilist[file_no--]);
	}
	return(c);
}
metacmd() { /* get next meta-graphic and interpret as possible
	include command byte */
	int c;
	char *name, *mstring();

	while ((c = metain()) == INCL) {
		name = mstring();
		if (++file_no < MAXINCLUDE) {
			if ((ilist[file_no] = fopen(name, "r")) == NULL) {
				error = E_INCL;
				file_no--;
			} else {
				xb[file_no] = base.x;
				yb[file_no] = base.y;
				base.x = base_x;
				base.y = base_y;
			}
		} else {
			error = E_INCLOV;
			file_no--;
		}
	}
	return(c);
}
static char str[MAXSTRING+1];
	char *
mstring() {
	register c, i;
	register char *s;

	s = str;
	for ( i = 0; (c = metain()) != '\0' && c != EOF ; )
		if (i < MAXSTRING) *s++ = c;
	*s = '\0';
	return(str);
}
	FILE * /* return file pointer to applications program */
ufile() { return ( (!file_no && direct) ? answr : NULL); }
	static void /* decode pen mapping */
pmapin(s) char *s; {
	int low, high, new;

	do {
		low = *s == '-' ? 0 : strtol(s, &s, 10);
		if (*s == '-') {
			++s;
			high = *s == ':' ? 255 : strtol(s, &s, 10);
		} else
			high = low;
		if (*s == ':') {
			++s;
			new = strtol(s, &s, 10);
		} else
			bomb(0,"improper -p list\n");
		for ( ; low <= high ; ++low)
			pmaplist[low] = new;
	} while (*s++ == ',');
}
