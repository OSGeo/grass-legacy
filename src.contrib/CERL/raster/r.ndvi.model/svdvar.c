void
svdvar(v,w,na,covar)
    double **v,w[],**covar;
    int na;
{
    int k,j,i;
    double sum,*wi,*vector();
    void free_vector();

    wi=vector(na);
    for (i=0;i<na;i++)
    {
        wi[i]=0.0;
        if (w[i])
	    wi[i]=1.0/(w[i]*w[i]);
    }
    for (i=0;i<na;i++)
    {
        for (j=1;j<=i;j++)
	{
	    sum = 0.0;
            for (k=0;k<na;k++)
		sum += v[i][k]*v[j][k]*wi[k];
            covar[j][i]=covar[i][j]=sum;
        }
    }
    free_vector(wi);
}
