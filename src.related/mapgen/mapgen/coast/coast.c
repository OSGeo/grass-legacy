#ifndef lint
static char *SCCSID = "@(#)coast.c	AMG v.3.3";
#endif
/* generate coastline overlay
** OLD parameter indicates that the old, slightly wrong version
** of the coast files are input
*/

# include <stdio.h>
# include <string.h>
# include "graphics.h"
# include "mapgen.h"
# include "coast.h"

static setpen(), scanner(), ln2_sin();

char	**eargv;
int	eargc;

FILE	*dir, *dat;	/* file pointers to coast line data */

char	*overlay,	/* overlay file name */
	*master,	/* map master file name */
	*ctl_file,	/* aux. control input name */
	*control;	/* string of run line control */

int	inter;		/* if set, do terminal graphics */

struct map_def def;

	static FILE *
fopenr(base, ext) char *base, *ext; {
	char name[80];
	FILE *f;

	strcpy(name, base);
	strcat(name, ext);
	if ((f = fopen(name, "r")) == NULL) {
		perror(name);
		return (NULL);
	}
	return f;
}
	static /* process coastline file */
cogen(file_base) char *file_base; {
	struct dircty w;
	long getpw(), tx;
	int nb, s, s0, s1;

		/* open up coast directory and data files */
	if ((dir = fopenr(file_base, ".cdr")) == NULL ||
		(dat = fopenr(file_base, ".cdt")) == NULL)
		emess(2, "can't process coastline files",(char *)0);

	while (getdir(&w, dir))
		/* window overlaps and selected feature code ? */
		if (!no_lap(w.lam_min, w.phi_min, w.lam_max, w.phi_max) &&
		   setpen(w.code)) {
			nb = ((w.cntrl >> 5) & 3) + 1;
			s = w.cntrl & 0x1f;
			s0 = ln2_sin(w.phi_base);
			fseek(dat, w.location, 0);
			newln(w.lam_base, w.phi_base);
# ifdef OLD
			w.count /= nb * 2;
# endif
			while (w.count--) {
				tx = getpw(nb,dat);
				w.phi_base += getpw(nb,dat) << s;
				s1 = ln2_sin(w.phi_base);
				w.lam_base += tx << ((s1 < s0 ? s1 : s0) + s);
				conln(w.lam_base, w.phi_base);
# ifndef OLD
				s0 = s1;
# endif
			}
		}
	fclose(dir);
	fclose(dat);
	return(0);
}

/* initialize coastline pen */

# define MAX_NAME 8
# define MAX_LINE 150
# define MAX_FC 30
# define DELIM	" \t"

struct plist {
	struct plist *next;
	short no_fc;
	char name[MAX_NAME+1];
	unsigned char fcs[MAX_FC], fce[MAX_FC];
};
	static struct plist
*plast,
*penlist;
	static int
lmode,
npen,
dash,
mpen;
# define MAX_FONT 60
# define MAX_STRING 9
	static char
lfont[MAX_FONT],
lstring[MAX_STRING];
	static double
lsize,
ldist,
size;
	static void
startpen() {
	struct plist *malloc(), *new;

	new = malloc(sizeof(struct plist));
	sprintf(new->name,"L%d",npen++);
	plotopt(NEWPEN, new->name);
	plotopt(WXL, def.D.x_min);
	plotopt(WXH, def.D.x_max);
	plotopt(WYL, def.D.y_min);
	plotopt(WYH, def.D.y_max);
	new->next = penlist;
	penlist = new;

}
	static void
endpen() {
	if (size > 0.) {
		plotopt(DMASK, (long)dash);
		plotopt(DSIZE, (long)(size * def.cts_cm));
		plotopt(DASH);
		size = 0.;
	}
	if (mpen) {
		plotopt(MPEN, (long)mpen);
		mpen = 0;
	}
	if (! penlist->no_fc) {
		penlist->fcs[penlist->no_fc] = 0;
		penlist->fce[penlist->no_fc++] = 255;
	}
	if (lsize && ldist) { /* symbol line mode */
		plotopt(FSYMS,lstring);
		plotopt(SFONTS,lfont);
		plotopt(F_SIZE, fcharsz(lsize * def.cts_cm / 21.));
		plotopt(F_DIST,(long)(ldist * def.cts_cm));
		plotopt(lmode ? FPLOT : FPLOTN );
	}
}
# define MAXC 50
	static void
peninit() {
	char line[MAX_LINE], *getline();
	FILE *file;
	char *argv[MAXC];

	startpen();

	if (control)
		words(control, MAXC, argv, scanner);

	if (ctl_file)
		if (file = fopen(ctl_file, "r"))
			while (control = getline(line, MAX_LINE, file)) {
				if (*control == '#') ++control;
				words(control, MAXC, argv, scanner);
			}
		else
			emess(2, "aux. control file", (char *)0);
	endpen();
}
	static
setpen(fc) unsigned fc; {
	struct plist *p;
	int i;

	for (p = penlist; p; p = p->next)
		for (i = 0; i < p->no_fc ; i++)
			if (fc >= p->fcs[i] && fc <= p->fce[i]) {
				if (p != plast) {
					plast = p;
					plotopt(SPEN, p->name);
				}
				return (1);
			}

	return (0); /* didn't find, so don't plot */
}
	static void
clrpen() { /* remove defined pens */
	struct plist *p;

	plotopt(DELPEN);
	for (p = penlist; p; p = p->next)
		if (p != plast) {
			plotopt(SPEN, p->name);
			plotopt(DELPEN);
		}
}
	static
scanner(argc, argv) char **argv; {
	char *arg, *s;
	unsigned fl, fh;
	int c;
	long strtol();
	double atof();

	while (argc-- > 0)
	if ((c = *(arg = *argv++)) == '-') for (;;) {
		switch (*++arg) { /* switch scan */
		case '\0':	/* null arg - start new pen */
			if (arg[-1] == '-') {
				endpen();
				startpen();
			}
			break;
		case 'a':	/* symbol line string */
			if (argc-- > 0)
				strncpy(lstring, *argv++, MAX_STRING-1);
			continue;
		case 'c':	/* symbol line font */
			if (argc-- > 0)
				strncpy(lfont, *argv++, MAX_FONT-1);
			continue;
		case 'd':	/* Dash size mask */
			if (argc-- > 0) {
				if (s = strchr(*argv, ',')) {
					if (dash = strtol(++s, 0, 0))
						size = atof(*argv);
					else
						size = 0.;
				}
				++argv;
			}
			continue;
		case 'f':	/* feature code */
			if (argc-- > 0) {
				for (s = *argv++; s ; ++s) {
					fl = strtol(s, &s, 0);
					if (s && *s == '-')
						fh = strtol(++s, &s, 0);
					else
						fh = fl;
					if (penlist->no_fc < MAX_FC) {
						penlist->fcs[penlist->no_fc]
							= fl;
						penlist->fce[penlist->no_fc++]
							= fh;
					}
					if (*s != ',')
						break;
				}
			}
			continue;
		case 'k':	/* no line with symbols */
			lmode = 0;
			continue;
		case 'l':	/* symbol line mode */
			lmode = 1;
			continue;
		case 'm':	/* mechanical pen */
			if (argc-- > 0)
				mpen = atoi(*argv++);
			continue;
		case 'r':	/* symbol line inter-char distance */
			if (argc-- > 0)
				ldist = atof(*argv++);
			continue;
		case 's':	/* symbol line char size */
			if (argc-- > 0)
				lsize = atof(*argv++);
			continue;
		}
		break;
	} else if (c == '#')
		break;
}
	long /* Input partial word.  Should work on any system */
getpw(n, file) int n; FILE *file; {
	long v;

	v = getc(file);
	if (v & 0x80) v = (v & 0xff) - 256;
	while (--n)
		v = (v << 8) + (getc(file) & 0xff);
	return v;
}
# define MAX_SHIFT 16
	static /* determine ln(base 2) of sin(pi - |x|) */
ln2_sin(x) long x; {
	register shift;

	if (x < 0)
		x = -x;
	x = IONE / (IHALF_PI - x);
	for (shift = 0; shift < MAX_SHIFT && (x >>= 1) ; )
		++shift;
	return shift;
}
	/* system entry */
main(argc, argv) char **argv; {
	char *arg;
	int i;

	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0': break;
			/* interactive display */
			case 'i': inter = 1;
				continue;
			/* set overlay file */
			case 'o': if(--argc > 0) overlay = *++argv;
				continue;
			/* map master file */
			case 'm': if (--argc > 0) master = *++argv;
				continue;
			/* supplementary control file */
			case 's': if (--argc > 0) ctl_file = *++argv;
				continue;
			/* line control data */
			case 'c': if (--argc > 0) control = *++argv;
				continue;
			default:
				fprintf(stderr,
					"invalid option %c\n", *arg);
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
		} else emess(1,"no map master given",(char *)0);
	if (! eargc)  /* no coastline sources specified */
		emess(1,"no coastline file[s]",(char *)0);
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
	peninit();
	/* process coastline files */
	for (i = 0; i < eargc; i++)
		if (cogen(eargv[i]))
			break;
	/* clear out pen(s) */
	clrpen();
	if (overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}
