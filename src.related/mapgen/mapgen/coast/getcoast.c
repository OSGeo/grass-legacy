#ifndef lint
static char *SCCSID = "@(#)getcoast.c	AMG v.3.3";
#endif
/* generate coastline overlay
** OLD parameter indicates that the old, slightly wrong version
** of the coast files are input
*/

# include <stdio.h>
# include <string.h>
# include "mapgen.h"
# include "coast.h"

# define MAXFC 20

static	ln2_sin();

char	**eargv;
int	eargc;

FILE	*dir, *dat;	/* file pointers to coast line data */

char	*delim = "# -b";	/* overlay file name */

int	rev,		/* if set, reverse to lon - lat output */
	dmsout;		/* if set, do DMS output */

struct map_def def;

struct {
	int lo, hi;
} list[MAXFC];
int nlist;

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
iproj(x, y, ox, oy) long x, y, *ox, *oy; {
	*ox = x;
	*oy = y;
}
	static
putdat(pen, b, a) long a, b; {
	double A, B;
	char va[20], vb[20], *rtodms();

	if (pen && delim)
		puts(delim);
	A = (rev ? b : a ) / DI_CON;
	B = (rev ? a : b ) / DI_CON;
	if (dmsout)
		printf("%s\t%s\n",
			rtodms(va, A, rev ? 'E' : 'N', rev ? 'W' : 'S'),
			rtodms(vb, B, rev ? 'N' : 'E', rev ? 'S' : 'W'));
	else
		printf("%.6f\t%.6f\n", A * R_TO_DEG, B * R_TO_DEG);
}
	static
inlist(fc) {
	int i;
	if (nlist) {
		for (i = 0; i < nlist; ++i)
			if (fc >= list[i].lo && fc <= list[i].hi)
				return(1);
		return(0);
	}
	return(1);
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
		   inlist(w.code)) {
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
	char *arg, *s;
	int i;
	double fl, fh, temp, dmstor();
	long strtol();

	eargv = argv;
	/* set ranges */
	if (argc < 6)
		emess(1,"insufficient arguments",(char *)0);
	if (--argc)
		def.l_lon = dmstor(*++argv,(char *)0);
	if (--argc)
		def.r_lon = dmstor(*++argv,(char *)0);
	if (def.l_lon > def.r_lon)
		def.l_lon -= TWO_PI;
	def.cm = (def.l_lon + def.r_lon) * .5;
	if (--argc)
		def.b_lat = dmstor(*++argv,(char *)0);
	if (--argc)
		def.t_lat = dmstor(*++argv,(char *)0);
	if (def.b_lat > def.t_lat) {
		temp = def.b_lat;
		def.b_lat = def.t_lat;
		def.t_lat = temp;
	}
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0': break;
			/* reverse output order */
			case 'r': rev = 1;
				continue;
			/* DMS output */
			case 'D': dmsout = 1;
				continue;
			/* delimiter */
			case 'b':
				delim = ++arg;
				if (!*delim)
					delim = (char *)0;
				continue;
		case 'f':	/* feature code */
			if (argc-- > 0) {
				for (s = *++argv; s ; ++s) {
					fl = strtol(s, &s, 0);
					if (s && *s == '-')
						fh = strtol(++s, &s, 0);
					else
						fh = fl;
					if (nlist < MAXFC) {
						list[nlist].lo = fl;
						list[nlist].hi = fh;
						++nlist;
					}
					if (*s != ',')
						break;
				}
			}
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
	if (! eargc)  /* no coastline sources specified */
		emess(1,"no coastline file[s]",(char *)0);
	def.D.x_min = def.D.y_min = 0x80000000L;
	def.D.x_max = def.D.y_max = 0x7fffffffL;
	geoinit(0, putdat, 0);
	nowrap(1);
	/* process coastline files */
	for (i = 0; i < eargc; i++)
		if (cogen(eargv[i]))
			break;
	exit(0);
}
