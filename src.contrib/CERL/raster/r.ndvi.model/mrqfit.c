mrqfit(x,nx,y,ndata,a,na,covar,alpha,chisq,funcs,lamda)
    double x[],y[],a[],**covar,**alpha,*chisq,*lamda;
    int nx,ndata,na;
    void (*funcs)(); /* see mrqcof() for definition */
{
    int k,j;
    static double *da,*dyda,*atry,**oneda,*beta,ochisq;
    double *vector(),**matrix();

    if (*lamda < 0.0)
    {
        oneda=matrix(na,1);
        atry=vector(na);
        da=vector(na);
        dyda=vector(na);
        beta=vector(na);
        *lamda=0.001;
        mrqcof(x,nx,y,ndata,a,na,alpha,beta,dyda,chisq,funcs);
        ochisq=(*chisq);
    }
    for (j=0;j<na;j++)
    {
        for (k=0;k<na;k++)
	    covar[j][k]=alpha[j][k];
        covar[j][j]=alpha[j][j]*(1.0+(*lamda));
        oneda[j][0]=beta[j];
    }
    gaussj(covar,na,oneda,1);
    for (j=0;j<na;j++)
        da[j]=oneda[j][0];
    if (*lamda == 0.0)
    {
        free_vector(beta);
        free_vector(dyda);
        free_vector(da);
        free_vector(atry);
        free_matrix(oneda);
        return;
    }
    for (j=0;j<na;j++)
        atry[j] = a[j]+da[j];
    mrqcof(x,nx,y,ndata,atry,na,covar,da,dyda,chisq,funcs);
    if (*chisq < ochisq)
    {
        *lamda *= 0.1;
        ochisq=(*chisq);
        for (j=0;j<na;j++)
	{
            for (k=0;k<na;k++)
		alpha[j][k]=covar[j][k];
            beta[j]=da[j];
            a[j]=atry[j];
        }
    }
    else
    {
        *lamda *= 10.0;
        *chisq=ochisq;
    }
    return;
}
