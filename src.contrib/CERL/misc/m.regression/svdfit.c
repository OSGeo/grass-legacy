#define TOL 1.0e-8

void
svdfit(x,y,ndata,a,na,u,v,w,basis)
			/* solve the fitting equations using singular value 
			decomposition method.
			x[1...ndata] and y[1...ndata] are observation
				points.
			fitting function is y = sum of [ a basis(x) ]
			u,v,w are working matrix, on output they define the 
				singular value decomposition and can be used to 
				obtain the covariance matrix.
			*/
    double x[],y[],a[],**u,**v,w[];
    int ndata,na;
    void (*basis)();	/* ANSI: void (*basis)(double,double *,int); */
{
    int j,i;
    double thresh,*b,*f,*vector();
    void svdcmp(),svbksb(),free_vector();
    b=vector(ndata);
    f=vector(na);
    for (i=0;i<ndata;i++)
    {
        (*basis)(x[i],f,na);
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
