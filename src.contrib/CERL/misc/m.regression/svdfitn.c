#define TOL 1.0e-8

void
svdfitn(x,nx,y,ndata,a,na,u,v,w,basis)

    double x[],y[],a[],**u,**v,w[];
    int ndata,nx,na;
    void (*basis)();	/* ANSI: void (*basis)(double *,int,,double *,int); */
{
    int j,i;
    double thresh,*xp,*b,*f,*vector();
    void svdcmp(),svbksb(),free_vector();
    b=vector(ndata);
    f=vector(na);

    for (xp=x,i=0;i<ndata;i++,xp+=nx)
    {
        (*basis)(xp,nx,f,na);
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
