#include <math.h>

#define SWAP(a,b) {double temp=(a);(a)=(b);(b)=temp;}

solve(a,b,n)
    double **a,*b;
    int n;
{
    int *indxc,*indxr,*ipiv;
    int i,icol,irow,j,k,h,hh,*ivector();
    double big,dum,pivinv;
    void nrerror(),free_ivector();

    indxc=ivector(n);
    indxr=ivector(n);
    ipiv=ivector(n);
    for (j=0;j<n;j++)
	ipiv[j]=0;
    for (i=0;i<n;i++)
    {
        big=0.0;
        for (j=0;j<n;j++)
            if (ipiv[j] != 1)
                for (k=0;k<n;k++)
		{
                    if (ipiv[k] == 0)
		    {
                        if (fabs(a[j][k]) >= big)
			{
                            big=fabs(a[j][k]);
                            irow=j;
                            icol=k;
                        }
                    }
		    else if (ipiv[k] > 1)
			nrerror("GAUSSJ: Singular Matrix-1");
                }
        ++(ipiv[icol]);
        if (irow != icol)
	{
            for (h=0;h<n;h++)
		SWAP(a[irow][h],a[icol][h])
	    SWAP(b[irow],b[icol])
        }
        indxr[i]=irow;
        indxc[i]=icol;
        if (a[icol][icol] == 0.0)
	    nrerror("GAUSSJ: Singular Matrix-2");
        pivinv=1.0/a[icol][icol];
        a[icol][icol]=1.0;
        for (h=0;h<n;h++)
	    a[icol][h] *= pivinv;
	b[icol] *= pivinv;
        for (hh=0;hh<n;hh++)
            if (hh != icol)
	    {
                dum=a[hh][icol];
                a[hh][icol]=0.0;
                for (h=0;h<n;h++)
		    a[hh][h] -= a[icol][h]*dum;
		b[hh] -= b[icol]*dum;
            }
    }
    for (h=n-1;h>=0;h--)
    {
        if (indxr[h] != indxc[h])
            for (k=0;k<n;k++)
                SWAP(a[k][indxr[h]],a[k][indxc[h]]);
    }
    free_ivector(ipiv);
    free_ivector(indxr);
    free_ivector(indxc);
}

#undef SWAP
