#define TOL 1.0e-8

void 
svdfit2 (
    double x1[],
    double x2[],
    double y[],
    int ndata,
    double a[],
    int na,
    double **u,
    double **v,
    double w[],
    void (*basis)(double,double,double *,int)
)
{
    int j,i;
    double thresh,*b,*f;

    b=vector(ndata);
    f=vector(na);
    for (i=0;i<ndata;i++) 
    {
        (*basis)(x1[i],x2[i],f,na);
        for (j=0;j<na;j++)
	    u[i][j]=f[j];
        b[i]=y[i];
    }
    svdcmp(u,ndata,na,w,v);
    thresh=0.0;
    for (j=0;j<na;j++)
        if (w[j] > thresh)
	    thresh=w[j];
    thresh *= TOL;
    for (j=0;j<na;j++)
        if (w[j] < thresh)
	    w[j]=0.0;
    svbksb(u,w,v,ndata,na,b,a);
    free_vector(f);
    free_vector(b);
}
