void svbksb(u,w,v,m,n,b,x)
    double **u,w[],**v,b[],x[];
    int m,n;
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
