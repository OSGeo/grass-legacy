# include <stdio.h>
# define MAX 10000
# define MAXLINE 100
int maxxyi = MAX;
	static float
*x, *y;
	static int
*ind;
	static char
head[MAXLINE],
data[MAXLINE];
main(argc, argv) char **argv; {
	int i, nind, verbose = 0, dclose = 0;
	double atof();
	float tol = 3.;
	extern char *optarg;

	while ((i = getopt(argc, argv, "dvt:s:")) != -1) 
		switch (i) {
		case 's': maxxyi = atoi(optarg);
		case 'd': dclose = 1; break;
		case 'v': verbose = 1; break;
		case 't': tol = atof(optarg); break;
		case '?':
			fprintf(stderr,"unknown arg\n");
			exit(1);
		}
	if (!(x = (float *)malloc(maxxyi*sizeof (float))) ||
	    !(y = (float *)malloc(maxxyi*sizeof (float))) ||
	    !(ind = (int *)malloc(maxxyi*sizeof (int)))) {
		fprintf(stderr,"allocation failure\n");
		exit(1);
	}
	if (verbose)
		fprintf(stderr,"tol: %g\n",tol);
	fgets(head, MAXLINE, stdin);
	for (;;) {
		if (*head != '#') {
			fprintf(stderr,"expected \"#\"\n");
			exit(1);
		}
		for (i = 0; i < maxxyi; ++i) {
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
			red2_(x, y, &i, &tol, ind, &nind);
			if (!(dclose && nind == 2 && strchr(head,'c') &&
				x[ind[0]-1] == x[ind[1]-1] &&
				y[ind[0]-1] == y[ind[1]-1])) {
				if (verbose) fprintf(stderr,"  out: %d\n",nind);
				fputs(head, stdout);
				for (i = 0; i < nind; ++i)
					printf("%g\t%g\n",
						x[ind[i]-1], y[ind[i]-1]);
			} else if (verbose)
				fprintf(stderr," -- deleted null closure\n");
		}
		if (feof(stdin))
			break;
		strcpy(head, data);
	}
	exit(0);
}
