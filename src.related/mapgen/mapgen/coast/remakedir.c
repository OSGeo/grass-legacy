#ifndef lint
static char *SCCSID = "@(#)remakedir.c	AMG v.3.1";
#endif
/*
**	regenerate binary image of the directory
*/

# include <stdio.h>
# include "mapgen.h"
# include "coast.h"

char	**eargv;
int	eargc;
FILE	*dir;

	static FILE *
fopenr(base, ext) char *base, *ext; {
	char name[80];
	FILE *f;

	strcpy(name, base);
	strcat(name, ext);
	if ((f = fopen(name, "r+")) == NULL) {
		perror(name);
		return (NULL);
	}
	return f;
}
	int
main(argc, argv) char **argv; {
	char *arg;
	char *file_base;

	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				break;

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

	if (! file_base) { /* no coastline file base named */
		fprintf(stderr,"no coastline file name\n");
		exit(1);
	}

		/* open up directory and data files */
	if ((dir = fopenr(file_base, ".cdr")) == NULL) {
		fprintf(stderr, "can't process cdr file\n");
		exit(1);
	}

	alter();

	fclose(dir);

	exit(0);
}
	static long
long_alt(x) long x; {
	char *s, *r;
	long V;
	int i;

	s = (char *) &x;
	r = (char *) &V;
	for (i = 0; i < 4; ++i)
		r[3-i] = s[i];
	return ( V );
}
	static
int_alt(x) int x; {
	char *s, *r;
	int i, V;

	s = (char *) &x;
	r = (char *) &V;
	for (i = 0; i < 2; ++i)
		r[1-i] = s[i];
	return ( V );
}
	static
alter() {
	struct dircty w;
	long loc = 0L;

	for (;;) {
		fseek(dir, loc, 0);
		if (!fread(&w, sizeof(struct dircty), 1, dir))
			return(0);
		w.count = int_alt(w.count);
		w.location = long_alt(w.location);
		w.lam_min = long_alt(w.lam_min);
		w.lam_max = long_alt(w.lam_max);
		w.phi_min = long_alt(w.phi_min);
		w.phi_max = long_alt(w.phi_max);
		w.lam_base = long_alt(w.lam_base);
		w.phi_base = long_alt(w.phi_base);
		fseek(dir, loc, 0);
		fwrite(&w, sizeof(struct dircty), 1, dir);
		loc = ftell(dir);
	}
}
