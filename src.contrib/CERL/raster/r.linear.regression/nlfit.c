/* non-linear least squares fit.
 *
 *  x[nx*ndata] input values
 *  nx          number of input variables
 *  ndata       number of data meausrements
 *  y           output values
 *  a           function parameters
 *  na          number of parameters
 *  chisq       computed for each call to nlfit()
 *  function    computes y and partials of y for each parameter
 *              called as function(x,nx,a,na,y,dyda)
 *                double x[nx], a[na], *y, dyda[na]
 *  state       if<0, initalize, if>0 normal run, if==0 done
 *
 * returns: 0 no chisq improvement, 1 chisq improved.
 *
 * the x array is organized: first nx elements are the first
 *   data point (nx variables), next nx elements are the second
 *   data point, etc.
 * must initalize the parameter array a before first call
 */
static double ochisq;
static double *dyda,*atry,*beta, *btemp;
static double **alpha, **atemp;
static double lamda;
static void (*fx)();
static int nx, na, ndata;

static int *indxc, *indxr, *ipiv; /* for solve */

static int first;
double *vector(),**matrix();
int *ivector();
double fabs();

nlsetup(ma,mx,mdata,functions)
    void (*functions)();
/*  void (*functions)(double *,int,double *,int,double *,double *); */
{
    na     = ma;
    nx     = mx;
    ndata  = mdata;
    fx     = functions;

    lamda  = 0.001;
    alpha  = matrix (na,na);
    atemp  = matrix (na,na);
    beta   = vector(na);
    btemp  = vector(na);
    atry   = vector(na);
    dyda   = vector(na);
    indxc  = ivector(na);
    indxr  = ivector(na);
    ipiv   = ivector(na);
    first = 1;
}
nlfinish()
{
    free_ivector(ipiv);
    free_ivector(indxr);
    free_ivector(indxc);
    free_vector(dyda);
    free_vector(atry);
    free_vector(btemp);
    free_vector(beta);
    free_matrix(atemp);
    free_matrix(alpha);
    return 0;
}

nlfit(x,y,a,chisq)
    double *x,*y,*a,*chisq;
{
    int k,j;

    if (first)
    {
	abc(x,y,a,alpha,beta,dyda,chisq);
	ochisq = *chisq;
	first = 0;
    }
    for (j = 0;j < na;j++)
    {
        for (k = 0;k < na;k++)
	    atemp[j][k] = alpha[j][k];
        atemp[j][j] = alpha[j][j]*(1.0+lamda);
    }

    solve2(atemp,beta);
    for (j = 0;j < na;j++)
        btemp[j] = beta[j];

    for (j = 0;j < na;j++)
        atry[j] = a[j]+btemp[j];
    abc(x,y,atry,atemp,btemp,dyda,chisq);
    if (*chisq < ochisq)
    {
        lamda *= 0.1;
        ochisq = *chisq;
        for (j = 0;j < na;j++)
	{
            for (k = 0;k < na;k++)
		alpha[j][k] = atemp[j][k];
            beta[j] = btemp[j];
            a[j] = atry[j];
        }
	return 1;
    }
    else
    {
        lamda *= 10.0;
        *chisq = ochisq;
	return 0;
    }
}

static
abc(x,y,a,alpha,beta,dyda,chisq)
    double *x,*y,*a,*beta,*dyda,*chisq;
    double **alpha;
{
    int k,j,i;
    double fy,wt,dy,*xp;

    for (j = 0; j < na;j++) 
    {
        for (k = 0; k <= j; k++)
	    alpha[j][k] = 0.0;
        beta[j] = 0.0;
    }
    *chisq = 0.0;
    for (xp = x, i = 0; i < ndata; i++, xp += nx)
    {
        (*fx)(xp,nx,a,na,&fy,dyda);
        dy = y[i]-fy;
        for (j = 0; j < na; j++)
	{
            wt = dyda[j];
            for (k = 0; k <= j; k++)
                alpha[j][k] += wt*dyda[k];
            beta[j] += dy*wt;
        }
        *chisq += dy*dy;
    }
    for (j = 1; j < na; j++)
        for (k = 0; k < j; k++)
	    alpha[k][j] = alpha[j][k];
}

#define SWAP(a,b) {double temp=(a);(a)=(b);(b)=temp;}

static
solve2(a,b)
    double **a,*b;
{
    int i,icol,irow,j,k,h,hh;
    double big,dum,pivinv;
    void nrerror();

    for (j=0;j<na;j++)
	ipiv[j]=0;
    for (i=0;i<na;i++)
    {
        big=0.0;
        for (j=0;j<na;j++)
            if (ipiv[j] != 1)
                for (k=0;k<na;k++)
		{
                    if (ipiv[k] == 0)
		    {
                        if (fabs(a[j][k]) >= big)
			{
                            big=fabs(a[j][k]);
                            irow=j;
                            icol=k;
                        }
                    }
		    else if (ipiv[k] > 1)
			nrerror("GAUSSJ: Singular Matrix-1");
                }
        ++(ipiv[icol]);
        if (irow != icol)
	{
            for (h=0;h<na;h++)
		SWAP(a[irow][h],a[icol][h])
	    SWAP(b[irow],b[icol])
        }
        indxr[i]=irow;
        indxc[i]=icol;
        if (a[icol][icol] == 0.0)
	    nrerror("GAUSSJ: Singular Matrix-2");
        pivinv=1.0/a[icol][icol];
        a[icol][icol]=1.0;
        for (h=0;h<na;h++)
	    a[icol][h] *= pivinv;
	b[icol] *= pivinv;
        for (hh=0;hh<na;hh++)
            if (hh != icol)
	    {
                dum=a[hh][icol];
                a[hh][icol]=0.0;
                for (h=0;h<na;h++)
		    a[hh][h] -= a[icol][h]*dum;
		b[hh] -= b[icol]*dum;
            }
    }
    for (h=na-1;h>=0;h--)
    {
        if (indxr[h] != indxc[h])
            for (k=0;k<na;k++)
                SWAP(a[k][indxr[h]],a[k][indxc[h]]);
    }
}

#undef SWAP
