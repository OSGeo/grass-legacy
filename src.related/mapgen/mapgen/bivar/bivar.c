#ifndef lint
static char *SCCSID = "@(#)bivar.c	AMG v.3.2";
#endif
# include <stdio.h>
# include <math.h>
# include "bivar.h"
# include "bieval.h"

char	**eargv;
int	eargc;

char	iform[20] = {"%le %le %le"}; /* normal format */
char	*uform;	/* user specified format */
char	*format;
static double	eta=1e-15, eps=1e-15, range=0.;
char	*outfil;	/* output file if other than sysout */
static int	d_size = 200, exch;
static best(), getdat();

BICOEF coef;
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
	int npts, b_deg;
	FILE *fid;
	int cnp(), mnp(), *ir, *apfs();
	double  ffct();
	static int vocal = 0;

	bfacts.np = coef.poly = mnp;
	bfacts.wghts = 3;
	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
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
				bfacts.wghts = 4;
				continue;

			/* tchebyshef polynomials */
			case 'c':
				coef.type = 1;
				bfacts.np = coef.poly = cnp;
				continue;

			/* exchange x-y order */
			case 'x':
				exch = 1;
				continue;

			/* verbosity */
			case 'v':
				vocal = 1;
				continue;

			default:
				break;
			}
			break;
		}
		else
			eargv[eargc++] = *argv;
	}
	if (outfil)
		if (freopen(outfil, "w", stdout) == NULL) {
			fprintf(stderr,"can't open output:%s\n",outfil);
			exit(1);
		}
	coef.x_deg = coef.y_deg = bfacts.deg;
		/* allocate memory */
	bfacts.xyzw = memory(d_size * bfacts.wghts);
	b_deg = ((bfacts.deg + 1)*(bfacts.deg + 2))/2;
	coef.Tx = bfacts.Tx = memory(bfacts.deg+1);
	coef.Ty = bfacts.Ty = memory(bfacts.deg+1);
	bfacts.p = memory(b_deg + 1);
	bfacts.work = memory(((b_deg+1)*(b_deg+2))/2);
		/* get data */
	if (eargc) {
		if ((fid = fopen(*eargv,"r")) == NULL) {
			fprintf(stderr,"can't open input:%s\n", *eargv);
			exit(1);
		}
	} else
		fid = stdin;
	format = (uform ? uform : iform);
	npts = getdat(fid, bfacts.xyzw, bfacts.wghts);
		/* scale data */

	if (vocal) fprintf(stderr,"%d points input\n", npts);
	if (setscale(bfacts.xyzw, bfacts.wghts, npts, range, &coef.x_scale) ||
	  setscale(bfacts.xyzw+1, bfacts.wghts, npts, range, &coef.y_scale)) {
		fprintf(stderr,"invalid data range\n");
		exit(1);
	}

	scalit(bfacts.xyzw, bfacts.wghts, &coef.x_scale, npts);
	scalit(bfacts.xyzw+1, bfacts.wghts, &coef.y_scale, npts);

		/* determine coefficients */
	apll(ffct, npts, b_deg, bfacts.p, bfacts.work);
	if (vocal) fprintf(stderr,"scaling & normal eqn's completed\n");

	ir = apfs(bfacts.work, b_deg, 2, eps, eta);
	if (vocal) fprintf(stderr,"ier: %d, ires: %d\n",*ir,ir[1]);

	best(ir[1]);
	if (vocal) fprintf(stderr,"BIVAR done\n\n");
}

static
getdat(fid, work, wghts)
FILE *fid;
double *work;
{
	int n;
	double t;

	for (n = 0;;) {
		if (wghts == 4 &&
			(fscanf(fid,format,work,work+1,work+2,work+3)
				!= wghts))
				break;
		else if (fscanf(fid,format,work,work+1,work+2) != wghts)
				break;
		if (exch) {
			t = *work;
			*work = work[1];
			work[1] = t;
		}
		n++;
		work += wghts;
	}
	return n;
}
	static
best(ir) {
	double *fw, eval();
	int i;

	fw = bfacts.work;
	for (i = 1; i <= ir; i++) {
		coef.maxC = i;
		coef.Cxy = fw;
		fw += i;
	}
	prbicoef(stdout, &coef);
}
