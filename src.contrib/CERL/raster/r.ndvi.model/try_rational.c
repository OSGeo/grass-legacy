double *vector();
double **matrix();
double rms(), sqrt(), err2(), evaluate();
double gasdev();
double ran1();
double fabs();
double deviation();
void rational();

static int idum = -1; /* for random number generator */

main(argc,argv) char *argv[];
{
    double **alpha, **covar, *a, *b, *dyda;
    double chisq, lambda;
    double *x,*y;
    double max;
    void rational();
    int i,j,k;
    int perturb = 1;
    int iterations;
    char ans[128];

    int nx = 2;        /* number of variables */
    int na = 2*(nx+1);	/* number of parameters */
    int ndata = 25; /* number of data points */

    for (i = 1; i < argc; i++)
	if (strcmp(argv[i], "-e") == 0)
	    perturb = 0;

    a = vector (na);    /* parameter array */
    dyda = vector (na); /* derivative array */
    b = a + nx + 1;
    x = vector (ndata*nx);
    y = vector (ndata);
    alpha = matrix (na,na);
    covar = matrix (na,na);

    for (i = 0; i < nx; i++)
    {
	b[i] = 1.0;
	a[i] = i%2 ? -1.0 : 1.0;
    }
    a[nx] = b[nx] = 0.0;

    k = 0;
    for (i = 0; i < ndata; i++)
	for (j = 0; j < nx; j++)
	    x[k++] = ran1(&idum);
    k = 0;
    for (i = 0; i < ndata; i++)
    {
	rational (x+k, nx, a, na, &y[i], dyda);
	for (j = 0; j < nx; j++)
	    printf ("x[%d]=%g ", j, x[k++]);
	printf ("y=%g", y[i]);
	if (perturb)
	{
	    y[i] += deviation();
	    printf (" (=%g)", y[i]);
	}
	printf ("\n");
    }

/* fit the model */
    lambda = -1.0;
    iterations = 1;
    while(iterations > 0)
    {
	while (iterations-- > 0)
	    mrqfit(x,nx,y,ndata,a,na,covar,alpha,&chisq,rational,&lambda);
	printf ("---------------\n");
	for (i = 0; i <= nx; i++)
	    printf (" a[%d]=%g", i,a[i]);
	printf ("\n");
	for (i = 0; i <= nx; i++)
	    printf (" b[%d]=%g", i,b[i]);
	printf ("\n");
	printf (" lambda=%g chisq=%g\n", lambda, chisq);

	while(1)
	{
	    printf ("iterations? ");
	    if (!gets(ans))
	    {
		iterations = 0;
		break;
	    }
	    if (sscanf (ans,"%d", &iterations)==1) break;
	}
    }
    lambda = 0.0;
    mrqfit(x,nx,y,ndata,a,na,covar,alpha,&chisq,rational,&lambda);
    printf ("------------------\n");
    printf ("covar matrix\n");
    for (i=0; i < na; i++)
    {
	for (j=0; j <= i; j++)
	    printf (" %12g", covar[i][j]);
	printf ("\n");
    }
}

double
err2(y1,y2,n)
    double *y1,*y2;
{
    double sum;
    double diff;
    int i;

    sum = 0.0;
    for (i=0; i<n;i++)
    {
	diff = y1[i]-y2[i];
	sum += diff*diff;
    }
    return sum;
}

double
rms(y1,y2,ndata)
    double *y1,*y2;
{
    return sqrt(err2(y1,y2,ndata))/ndata;
}

double
deviation()
{
    return gasdev(&idum) * 2.0;
}
