int mrqcof (double x[], int nx, double y[], int ndata, double a[], int na,
    double **alpha, double beta[], double dyda[], double *chisq,
    void (*funcs)(double *,int,double *,int,double *,double *))
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

    return 0;
}
