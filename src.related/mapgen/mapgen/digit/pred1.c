# include <stdio.h>
# define MAXLINE 100
	static float
*x, *y;
	static int
MAX = 50000,
*ind;
	static char
head[MAXLINE],
data[MAXLINE];
main(argc, argv) char **argv; {
	int i, nind, verbose = 0;
	double atof();
	char *malloc();
	float tol = 3.;
	extern char *optarg;

	while ((i = getopt(argc, argv, "vt:s:")) != -1) 
		switch (i) {
		case 'v': verbose = 1; break;
		case 't': tol = atof(optarg); break;
		case 's': MAX = atoi(optarg); break;
		case '?':
			fprintf(stderr,"unknown arg\n");
			exit(1);
		}
	if (!(x = (float *)malloc(MAX * sizeof(float))) ||
		!(y = (float *)malloc(MAX * sizeof(float))) ||
		!(ind = (int *)malloc(MAX * sizeof(int))) ) {
		fprintf(stderr,"failed to allocate memory\n");
		exit(1);
	}
	if (verbose) fprintf(stderr,"tol: %g\n",tol);
	fgets(head, MAXLINE, stdin);
	for (;;) {
		if (*head != '#') {
			fprintf(stderr,"expected \"#\"\n");
			exit(1);
		}
		for (i = 0; i < MAX; ++i) {
			if (!fgets(data, MAXLINE, stdin))
				break;
			if (*data == '#')
				break;
			if (sscanf(data,"%g %g",x+i, y+i) != 2) {
				fprintf(stderr,"scan error\n");
				exit(1);
			}
		}
		if (i) {
			if (verbose) fprintf(stderr,"in pts: %d",i);
			fputs(head, stdout);
			red1_(x, y, &i, &tol, ind, &nind);
			if (verbose) fprintf(stderr,"  out: %d\n",nind);
			for (i = 0; i < nind; ++i)
				printf("%g\t%g\n",x[ind[i]-1], y[ind[i]-1]);
		}
		if (*data != '#')
			break;
		strcpy(data, head);
	}
	exit(0);
}
