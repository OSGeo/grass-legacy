/* digitizer to linear conversion */

# include <stdio.h>
# include <string.h>
# include <math.h>

# define DATA 0
# define USER 1
# define DIGIT 2
	static int
con_chr = '#',
verbose = 0,
nuser = 0,
mode = USER,
deg = 1,
ncal = 0;
	static double
tol = 0,
xlast = 999999,
ylast = 999999;
	static char
informat[] = "%le\t%le\t%c";
# define MAX_CAL 50
# define MAX_DEG 8
	static double
xc[MAX_CAL], yc[MAX_CAL], /* calibration quadruple */
xu[MAX_CAL], yu[MAX_CAL],
# define MAX_C ((MAX_DEG+1)*(MAX_DEG+2))/2
x_scale, x_off, y_scale, y_off, xC[MAX_C], yC[MAX_C];

main(argc, argv) char **argv; {
	FILE	*fid;
	char	*arg, **eargv;
	static int eargc = 0;

	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if (arg[-1] == '-')
					eargv[eargc++] = "-";
				break;
				/* set alternate output */
			case 'o':
				if(argc-- <= 0)
					continue;
				if (freopen(*++argv, "w", stdout) == NULL) {
					perror("can't change output");
					exit(1);
				} else {
					fprintf("no output file given\n");
					exit(1);
				}
				continue;
			case 'd': /* set degree of fit */
				if (argc-- > 0) {
					deg = atoi(*++argv);
					if (deg > MAX_DEG)
						deg = MAX_DEG;
					else if (deg < 0)
						deg = 0;
				}
				continue;
			case 't': /* set tolerance */
				if (argc-- > 0) {
					tol = atoi(*++argv);
					if (tol < 0) tol = 0;
				}
				continue;
			case 'v':
				verbose = 1;
				continue;
			case 'z': /* set control line character */
				if (arg[1]) con_chr = *++arg;
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
		 /* no line sources specified */
	if (! eargc)
		eargv[eargc++] = "-";
		/* process line files */
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			perror(*eargv);
			continue;
		}
		process(fid);
	}
	exit(0);
}
# define ONE_EPS	1.001
	static double
Tx[MAX_DEG+1], Ty[MAX_DEG+1];
	static
scale(x, y, xp, yp) double x, y, *xp, *yp; {
	register double *Cx, *Cy;
	double c;
	register i, j;
		/* reduce range & scale */
	x = x * x_scale + x_off;
	y = y * y_scale + y_off;
	if (deg > 1 && (fabs(x) > ONE_EPS || fabs(y) > ONE_EPS))
		return(1);
		/* form univariate vectors */
	cnp(Tx, x, deg);
	cnp(Ty, y, deg);
		/* evaluate product */
	Cx = xC;
	Cy = yC;
	for (*xp = *yp = 0., i = 0; i <= deg ; i++)
		for (j = 0; j <= i; j++) {
			c = Tx[i-j] * Ty[j];
			*xp += c * *Cx++;
			*yp += c * *Cy++;
		}
	return 0;
}
	/* chebyshev polynomial coefficients */
	static
cnp(t, x, n) double t[]; register double x; {
	register i;

	t[0] = 1.;
	if ( n > 0 ) {
		t[1] = x;
		x += x;
		for ( i = 1 ; i < n ; ++i )
			t[i + 1] = x * t[i] - t[i - 1];
	}
}
	/* load coefficients */
	static
loadscl(C, fi, n) double *C; FILE *fi; {
	int i;
	fscanf(fi,"%*d %*d %*d %d",&i);
	if (i != n) {
		fprintf(stderr,
		"conversion failure on degree (%d), retn'd %d\n", n, i);
		return (1);
	}
	fscanf(fi,"%lf %lf %*f %*f %lf %lf %*f %*f",&x_scale,&x_off,
		&y_scale, &y_off);
	for (i = 0; i < n ; ++i)
		fscanf(fi, "%lf", &C[i]);
	return (0);
}
	static
calibr() {
	int n, d, e;
	FILE *ffo, *pfopen();
	static char *args[] = {
		"/usr/local/bin/bivar",
		"bivar",
		"-cmf",
		"0",
		"%le %le %le %*e",
		0,
		0 };
	static char *fmt = "%.15e %.15e %.15e %.15e\n";
	d = ((deg + 1)*(deg + 2)) / 2;
	*args[3] += deg;
	if (ncal >= d && nuser == ncal) {
		n = ncal;
		args[5] = tempnam("/tmp","to");
		ffo = fopen(args[5], "w");
		while (n--)
			fprintf(ffo, fmt, xc[n], yc[n], xu[n], yu[n]);
		fclose(ffo);
		ffo = pfopen(args,"r");
		e = loadscl(xC, ffo, d);
		pfclos(ffo);
		args[4] = "%le %le %*e %le";
		ffo = pfopen(args,"r");
		e += loadscl(yC, ffo, d);
		pfclos(ffo);
		unlink(args[5]);
		if (e) exit(1);
		if (verbose) { /* list residual of calibration */
			double x, y;
			fprintf(stderr,"calibration residual\n");
			fprintf(stderr,"degree: %d, no. coeff: %d\n",
				deg, d);
			for (n = 0; n < ncal; ++n) {
				scale(xc[n], yc[n], &x, &y);
				fprintf(stderr,"%.2e\t%.2e\n",
					xu[n]-x, yu[n]-y);
			}
		}
	} else {
		fprintf(stderr,"cal. failure (req: %d), user: %d, digit: %d\n",
			d, nuser, ncal);
		exit(1);
	}
}
	static
convert(x, y, key) double x, y; {
	double xv, yv;

	if (scale(x, y, &xv, &yv))
		printf("*\t*\t%c\n",key);
	else
		printf("%.12le\t%.12le\t%c\n",xv,yv,key);
}
	/* user coordinate input */
	static
douser(argc, argv) char **argv; int argc; {
	double atof();

	if (argc > 1 && nuser < MAX_CAL) {
		xu[nuser] = atof(*argv++);
		yu[nuser++] = atof(*argv);
	}
}
	static
dopoint(s) char *s; {
	double x, y;
	int j, n;
	char key;

	if ((n = sscanf(s, informat, &x, &y, &key)) >= 2 &&
		(!tol || fabs(x - xlast) >= tol || fabs(y - ylast) >= tol)) {
		if (mode == DIGIT) {
			if (n == 3 && key != '1' && ncal > 0) ncal--;
			if (ncal < MAX_CAL) {
				xc[ncal] = x;
				yc[ncal++] = y;
			}
		} else
			convert(x, y, key);
		xlast = x;
		ylast = y;
	}
}

# define MAX_LINE 250
# define MAXC 50
	static char
line[MAX_LINE],
*argv[MAXC];
	static
process(fid) FILE *fid; {
	char *s, *getline();

	while (s = getline(line, MAX_LINE, fid))
		if (*s == con_chr)
			switch (mode) {
			case USER:
				mode = DIGIT;
				break;
			case DIGIT:
				calibr();
				mode = DATA;
				break;
			default:
				xlast = ylast = 9999999.;
				puts(line);
				break;
			}
		else if (mode == USER)
			words(s, MAXC, argv, douser);
	 	else
			dopoint(s);
}
