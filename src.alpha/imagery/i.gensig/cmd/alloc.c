#include "gis.h"

double *vector(nl,nh)
    int nl,nh;
{
    double *v;

    v=(double *)G_malloc((unsigned) (nh-nl+1)*sizeof(double));
    return v-nl;
}

double **matrix(nrl,nrh,ncl,nch)
    int nrl,nrh,ncl,nch;
{
    int i;
    double **m;

    m=(double **) G_malloc((unsigned) (nrh-nrl+1)*sizeof(double*));
    m -= nrl;

    for(i=nrl;i<=nrh;i++) {
	    m[i]=(double *) G_malloc((unsigned) (nch-ncl+1)*sizeof(double));
	    m[i] -= ncl;
    }
    return m;
}

free_vector(v,nl,nh)
    double *v;
    int nl,nh;
{
    free((char*) (v+nl));
}

free_matrix(m,nrl,nrh,ncl,nch)
    double **m;
    int nrl,nrh,ncl,nch;
{
    int i;

    for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
    free((char*) (m+nrl));
}
