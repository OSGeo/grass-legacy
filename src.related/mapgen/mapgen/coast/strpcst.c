#ifndef lint
static char *SCCSID = "@(#)strpcst.c	AMG v.3.2";
#endif
/* generate ascii coastline file from packed file */

# include <stdio.h>
# include "mapgen.h"
# include "coast.h"

/*
# define INT_DEG	.00000057295779513082320876798
*/

static dump();

	static int
degrees, hex, head;
	static char
*f1[] = {
	" lon from %ld to %ld, lat from %ld to %ld\n",
	" lon from %lx to %lx, lat from %lx to %lx\n" },
*f2[] = {
	"%10ld %10ld\n",
	"%8lx %8lx\n" };
char	**eargv;
int	eargc;
FILE	*dir, *dat;

	/* determine ln(base 2) of sin(pi - |x|) */
# define MAX_SHIFT 16
	static int
ln2_sin(x) long x; {
	int shift = 0;

	x = IONE / (IHALF_PI + (x > 0 ? -x : x));
	while ( shift < MAX_SHIFT && (x >>= 1) ) ++shift;
	return shift;
}
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
	long /* Input partial word.  Should work on any system */
getpw(n, file) int n; FILE *file; {
	long v;

	if ((v = getc(file)) & 0x80) v = (v & 0xff) - 256;
	while (--n) v = (v << 8) + (getc(file) & 0xff);
	return v;
}
main(argc, argv) char **argv; {
	char *arg;
	char *file_base;

	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				break;
			case 'd': /* -d > selects degrees output */
				degrees = 1;
				continue;
			case 'x': /* -x > selects hex listing */
				hex = 1;
				continue;
			case 'h': /* -h > print header only */
				head = 1;
				continue;
			/* user input file base name */
			case 'f': /* -f file > selects "coastline" file */
				if(--argc > 0)
					file_base = *++argv;
				continue;
			default:
				break;
			}
			break;
		}
		else
			eargv[eargc++] = *argv;
	}

	if (! eargc) { /* no specified output so 'sysout' only */
		eargc = 1;
		eargv[0] = "-";
	}

	if (! file_base) { /* no coastline file base named */
		fprintf(stderr,"no coastline file name\n");
		exit(1);
	}

		/* open up directory and data files */
	if ((dir = fopenr(file_base, ".cdr")) == NULL ||
		(dat = fopenr(file_base, ".cdt")) == NULL) {
		fprintf(stderr, "can't process cdr/cdt files\n");
		exit(1);
	}

	dump();

	fclose(dat);
	fclose(dir);

	exit(0);
}
	static
dump() {
	struct dircty w;
	/*
	long tx, x[MAX_XY], y[MAX_XY], getpw();
	*/
	long tx;
	int nb, s, s0, s1;

	for (;;) {
		if (!fread(&w, sizeof(struct dircty), 1, dir))
			return(0);

		nb = ((w.cntrl >> 5) & 3) + 1;
		s = w.cntrl & 0x1f;
		printf("res: %d, %nb: %d, code: %d, pts: %u",
			s,nb,w.code,w.count);

		if (degrees) {
			printf(" lon from %.8f to %.8f, lat from %.8f to %.8f\n"
				,INT_DEG*w.lam_min,INT_DEG* w.lam_max,
				INT_DEG* w.phi_min,INT_DEG* w.phi_max);

			printf("%15.8f %15.8f\n",INT_DEG*w.lam_base,
				INT_DEG*w.phi_base);
		} else {
			printf(f1[hex],
				w.lam_min, w.lam_max, w.phi_min, w.phi_max);

			printf(f2[hex],w.lam_base,w.phi_base);
		}
		s0 = ln2_sin(w.phi_base);
		if (head)
			continue;
		fseek(dat, w.location, 0);
		while (w.count--) {
			tx = getpw(nb, dat);
			w.phi_base += getpw(nb, dat) << s;
			s1 = ln2_sin(w.phi_base);
			w.lam_base += tx << ((s1 < s0 ? s1 : s0) + s);
			if (degrees)
				printf("%15.8f %15.8f\n",INT_DEG*w.lam_base,
					INT_DEG*w.phi_base);
			else
				printf(f2[hex],w.lam_base,w.phi_base);
			s0 = s1;
		}
	}
}
