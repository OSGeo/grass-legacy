mrqcof(x,nx,y,ndata,a,na,alpha,beta,dyda,chisq,funcs)
    double x[],y[],a[],**alpha,beta[],dyda[],*chisq;
    int nx,ndata,na;
    void (*funcs)();
/* ANSI:
    void (*funcs)(double *,int,double *,int,double *,double *);
*/
{
    int k,j,i;
    double fy,wt,dy,*xp;

    for (j=0;j<na;j++) 
    {
        for (k=0;k<=j;k++)
	    alpha[j][k]=0.0;
        beta[j]=0.0;
    }
    *chisq=0.0;
    for (xp=x,i=0;i<ndata;i++,xp+=nx)
    {
        (*funcs)(xp,nx,a,na,&fy,dyda);
        dy=y[i]-fy;
        for (j=0;j<na;j++)
	{
            wt=dyda[j];
            for (k=0;k<=j;k++)
                alpha[j][k] += wt*dyda[k];
            beta[j] += dy*wt;
        }
        (*chisq) += dy*dy;
    }
    for (j=1;j<na;j++)
        for (k=0;k<j;k++)
	    alpha[k][j]=alpha[j][k];
}
