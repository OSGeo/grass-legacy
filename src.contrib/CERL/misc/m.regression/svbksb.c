void 
svbksb (double **u, double w[], double **v, int m, int n, double b[], double x[])
{
    int j,i;
    double s,*tmp,*vector();
    void free_vector();

    tmp=vector(n);
    for (j=0;j<n;j++) {
        s=0.0;
        if (w[j]) {
            for (i=0;i<m;i++) s += u[i][j]*b[i];
            s /= w[j];
        }
        tmp[j]=s;
    }
    for (j=0;j<n;j++) {
        s=0.0;
        for (i=0;i<n;i++) s += v[j][i]*tmp[i];
        x[j]=s;
    }
    free_vector(tmp);
}
