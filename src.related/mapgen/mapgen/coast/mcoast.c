#ifndef lint
static char *SCCSID = "@(#)mcoast.c	AMG v.3.2";
#endif
/*
**	Generate coastline directory-data files from generic coastal
**	information.
**
**	This generic version expects the following information from
**	the 'stdin' stream:
**		segment header: '#' in col. 1 followed by feature code
**		node data record: latitude <white_space> longitude in DMS
**	Segment terminated by either a '#' record for next segment or EOF
**
**	Segment node length determined by MAX_XY
*/

# include <stdio.h>
# include <math.h>
# include "coast.h"

# define MAX_XY 16100

extern long strtol();
static char *fname;
static makecst();
static int count;
static int binin; /* binary input flag */
char	**eargv;
int	eargc;
FILE	*dir, *dat, *fin;

static long *x, *y;
static int reverse;
	static
usage() { fprintf(stderr, "mcoast -f file [-r n] [-R]\n"); exit(1); }
	static FILE *
fopenr(base, ext) char *base, *ext; {
	char name[80];
	FILE *f;

	strcpy(name, base);
	strcat(name, ext);
	if ((f = fopen(name, "w+")) == NULL) {
		perror(name);
		return (NULL);
	}
	return f;
}
main(argc, argv) char **argv; {
	long res, *malloc(), t;
	int i, err;
	char *file_base, *arg;

	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if (arg[-1] == '-')
					eargv[eargc++] = "-";
				break;
			/* binary input stream */
			case 'b':
				binin = 1;
				continue;
			/* data resolution */
			case 'r':
				if (--argc > 0)
					res = strtol(*++argv, (char *)0, 10);
				continue;
			/* user output file base name */
			case 'f':
				if(--argc > 0)
					file_base = *++argv;
				continue;
			/* reverse from lat lon to lon lat */
			case 'R':
				reverse = 1;
				continue;
			default:
				fprintf(stderr,
					"invalid option %c\n", *arg);
				usage();
				break;
			}
			break;
		} else
			eargv[eargc++] = *argv;
	}
	if (! eargc) { /* no specified input so 'sysin' only */
		eargc = 1;
		eargv[0] = "-";
	}
	if (! file_base) { /* no coastline file base named */
		fprintf(stderr,"no coastline file name\n");
		usage();
	}
		/* open up directory and data files */
	if ((dir = fopenr(file_base, ".cdr")) == NULL ||
		(dat = fopenr(file_base, ".cdt")) == NULL) {
		fprintf(stderr, "can't process cdr/cdt files\n");
		exit(1);
	}
		/* set resolution */
	set_res(res);
		/* allocate work arrays */
	t = (long)sizeof(long) * MAX_XY;
	if (!(x = malloc((unsigned)t)) || !(y = malloc((unsigned)t))) {
		fprintf(stderr, "can't allocate memory\n");
		exit(1);
	}
		/* process input file[s] */
	for (i = err = 0; i < eargc && !err; i++) {
		fname = eargv[i];
		if (! strcmp(fname, "-")) {
			fin = stdin;
			fname = "<stdin>";
		} else if ((fin = fopen(fname,"r")) == NULL) {
			perror(eargv[i]);
			fprintf(stderr, "can't process input file\n");
			exit(1);
		}
		count = 0;
		err = makecst();
		fclose(fin);
	}
	fclose(dat);
	fclose(dir);
	exit(0);
}

# define CONV	1e8
# define DCONV	1745329.2519943
# define MAXLINE 100

	static long
lrnd(x) double x; { return ( x < 0. ? x - .5 : x + .5); }

# define DUMB ++count ? MAXLINE : MAXLINE
	static
bput(v, n, dat) long v; FILE *dat; {
	static char w[4];
	int i = n;

	while (i) { w[--i] = v; v >>= 8; }
	while (i < n) putc(w[i++], dat);
}
# define dx r.x
# define dy r.y
	static
makecst() {
	struct dircty *w, *xypack();
	int npts, len, i, code, nb;
	char *s, line[MAXLINE];
	double dmstor();
	struct {
		double x, y;
	} r;

	for (s = fgets(line, DUMB, fin); s ; ) {
		if (*s == '#') { /* header */
			code = strtol(++s, &s, 10);
			if (binin) {
				len = strtol(s, (char *)0, 10);
				if (len > MAX_XY) {
					fprintf(stderr,
						"segment too long (%d)\n",len);
					exit(1);
				}
			} else
				len = MAX_XY;
			/* get node data */
			for (npts = 0; npts < len ; ++npts) {
				if (binin) {
					if (!fread(&r, sizeof(r), 1, fin))
						break;
					x[npts] = lrnd(DCONV * dx);
					y[npts] = lrnd(DCONV * dy);
				} else {
					if (!(s = fgets(line,DUMB,fin)) ||
						*s == '#')
					break;
					if (((dy = dmstor(s, &s)) != HUGE &&
						*s && (dx=dmstor(s, (char *)0))
						!= HUGE)) {
						x[npts] = lrnd(CONV *
							(reverse?dy:dx));
						y[npts] = lrnd(CONV *
							(reverse?dx:dy));
					} else {
						printf("%d rec on %s <%s>\n",
							count,fname,line);
						--npts;
					}
				}
			}
			if (npts <= 0) {
			printf("%d rec on %s null\n",count,fname);
			continue;
			} else if (npts == 1) { /* force point plot */
				printf("%d rec on %s single pt.\n",count,fname);
				x[1] = x[0];
				y[1] = y[0];
				++npts;
			}
			w = xypack(x, y, npts);
			w->code = code;
			nb = ((w->cntrl >> 5) & 3) + 1;
			for (i = 0, --npts; i < npts; i++) {
				bput(x[i], nb, dat);
				bput(y[i], nb, dat);
			}
			putdir(w, dir);
		} else
			s = fgets(line, DUMB, fin);
	}
	return(0);
}
