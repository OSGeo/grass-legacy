#ifndef lint
static char *SCCSID = "@(#)univar.c	AMG v.3.1";
#endif
# include <stdio.h>
# include "univar.h"

char	**eargv;
int	eargc;
char	*outfil;

char	iform[20] = {"%le %le"}; /* normal format */
char	*uform;	/* user specified format */
char	*format;
static double	eta=1e-15, eps=1e-15, range=0.1, atof();
char	*outfil;	/* output file if other than sysout */
int	d_size = 200;

	static double *
memory(n) {
	double *m, *malloc();

	if ((m = malloc(n * sizeof(double))) == NULL) {
		fprintf(stderr,"can't allocate memory\n");
		exit(1);
	}

	return m;
}

main(argc, argv) char **argv; {
	char *arg;
	int npts, i, b_deg;
	FILE *fid;
	struct SCALE xs, *setscale();
	int cnp(), mnp(), *ir, *apfs();
	double *fw, eval(), ffct();

	bfacts.np = mnp;
	bfacts.wghts = 2;
	eargv = argv;
	while (--argc > 0) {
		if (**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				break;

			/* epsilon */
			case 'e':
				if (--argc > 0)
					eps = atof(*++argv);
				continue;

			/* eta */
			case 't':
				if (--argc > 0)
					eta = atof(*++argv);
				continue;

			/* range factor */
			case 'r':
				if (--argc > 0)
					range = atof(*++argv);
				continue;

			/* maximum degree */
			case 'm':
				if (--argc > 0)
					bfacts.deg = atoi(*++argv);
				continue;

			/* maximum data size */
			case 's':
				if (--argc > 0)
					d_size = atoi(*++argv);
				continue;

			/* alternate output */
			case 'o':
				if(--argc > 0)
					outfil = *++argv;
				continue;

			/* user format */
			case 'f':
				if(--argc > 0)
					uform = *++argv;
				continue;

			/* weight factors */
			case 'w':
				strcat(iform," %le");
				bfacts.wghts = 3;
				continue;

			/* tchebyshef polynomials */
			case 'c':
				bfacts.np = cnp;
				continue;

			default:
				break;
			}
			break;
		}
		else
			eargv[eargc++] = *argv;
	}
		/* allocate memory */
	bfacts.xzw = memory(d_size * bfacts.wghts);
	b_deg = bfacts.deg + 1;
	bfacts.Tx = memory(b_deg + 1);
	bfacts.p = memory(b_deg + 1);
	bfacts.work = memory(((b_deg+1)*(b_deg+2))/2);
		/* get data */
	if (eargc) {
		if ((fid = fopen(*eargv,"r")) == NULL) {
			fprintf(stderr,"can't open input:%d\n", *eargv);
			exit(1);
		}
	} else
		fid = stdin;
	format = (uform ? uform : iform);
	npts = getdat(fid, bfacts.xzw, bfacts.wghts);
		/* scale data */
	if (setscale(bfacts.xzw, bfacts.wghts, npts, range, &xs)) {
		fprintf(stderr,"invalid data range\n");
		exit(1);
	}
	scalit(bfacts.xzw, bfacts.wghts, &xs, npts);
		/* determine coefficients */
	apll(ffct, npts, b_deg, bfacts.p, bfacts.work);
pgroup(bfacts.work,((b_deg+1)*(b_deg+2))/2);
	ir = apfs(bfacts.work, b_deg, 2, eps, eta);
	printf("%d %d\n",*ir,*(ir+1));
	printf("%.12e %.12e %.12e %.12e\n",
		xs.scale, xs.off, xs.min, xs.max);
	fw = bfacts.work;
	for (i = 1; i <= *(ir+1); i++) {
		eval(npts, fw, i);
		pgroup(fw,i);
		fw += i;
	}
}
	static
getdat(fid, work, wghts) FILE *fid; double *work; {
	int n;

	for (n = 0;;) {
		if (wghts == 3 &&
			(fscanf(fid,format,work,work+1,work+2)
				!= wghts))
				break;
		else if (fscanf(fid,format,work,work+1) != wghts)
				break;
		n++;
		work += wghts;
	}
	return n;
}
pgroup(ptr, l) double *ptr; {
	int i;

	i = 0;
	for (; l--; ptr++) {
		if (!i) printf("\n");
		i = (i+1)%4;
		printf(" %.12e",*ptr);
	}
	printf("\n");
}

/* ffct - generate bfactsiate observational vector for 'apll'
**	'i'th observation (1 to --)
**	'p' loaded with fundamental equation values followed by
**		functional value.
**	returns weight associated with value.
**
**	structure 'bfacts' must, of course, be already set up.
*/
	double
ffct(i, p) double p[]; {
	double *q;

	q = bfacts.xzw + (i-1) * bfacts.wghts;

		/* form univariate vectors */
	(*bfacts.np)(p, *q++, bfacts.deg);

	p[bfacts.deg + 1] = *q;
		/* return weight */
	if (bfacts.wghts > 2)
		return( *++q );
	else
		return( 1. );
}
	static double
vprod(n, nC, Tx, Cx) double *Tx, *Cx; {
	register double v;
	register i;

	v = 0.;
	for (i = 0; i <= n && i < nC; i++) {
		v += *Tx++ * *Cx++;
	}
	return( v );
}
	double
eval(npts, Cx, nC) double *Cx; {
	double *q, v;

	q = bfacts.xzw;
	while (npts--) {
		/* form univariate vectors */
printf("%e ",*q);
		(*bfacts.np)(bfacts.Tx, *q++, bfacts.deg);

		/* form bivariate vector */
		v = vprod(bfacts.deg, nC, bfacts.Tx, Cx);
printf("value, orig: %e %e\n",v,*q++);
		if (bfacts.wghts > 2) q++;
	}
	return v;
}
