#ifndef lint
static char *SCCSID = "@(#)todeg.c	AMG v.3.1";
#endif
/* convert delta file data to degrees
*/
# include <stdio.h>

/* 1./3600 */
# define CV .00027777777777777777
static double conver = CV;
main(argc, argv) char **argv; {
	long x, y, xl, yl;
	struct {
		double x, y;
	} r;
	int cnt, ascout;
	char *s, *strrchr(), line[100];
	extern double pow();

	if (s = strrchr(*argv, '/')) *argv = s + 1;
	ascout = strcmp(*argv, "tobin");

	if (argc > 1 && *argv[1] == '-') {
		if ((cnt = *(argv[1]+1)) == 'd' || cnt == 's') {
			if (cnt == 'd')
				conver = 1.;
			if (cnt = atoi(argv[1]+2))
				conver /= pow(10., (double) cnt);
		} else {
			fprintf(stderr,"invalid arg: %s\n",argv[1]);
			exit(1);
		}
	}

	while (gets(line)) {
		if (*line != '#') {
			fprintf(stderr,"expected '#' record\n");
			exit(1);
		}
		puts(line);
		sscanf(line,"# %*d %d",&cnt);
		xl = yl = 0L;
		while (cnt --) {
			if (! gets(line)) {
				fprintf(stderr,"EOF on line seg\n");
				exit(1);
			}
			if (*line == '#') {
				fprintf(stderr,"'#' on line seg\n");
				exit(1);
			}
			sscanf(line,"%ld\t%ld",&y,&x);
			r.y = conver * (yl += y);
			r.x = conver * (xl += x);
			if (ascout)
				printf("%.6f\t%.6f\n", r.y, r.x);
			else
				fwrite(&r, sizeof(r), 1, stdout);
		}
	}
}
