#define TOL 1.0e-8

void
svdfit2(x1,x2,y,ndata,a,na,u,v,w,basis)
    double x1[],x2[],y[],a[],**u,**v,w[];
    int ndata,na;
    void (*basis)();	/* ANSI: void (*basis)(double,double,double *,int); */
{
    int j,i;
    double thresh,*b,*f,*vector();
    void svdcmp(),svbksb(),free_vector();

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
