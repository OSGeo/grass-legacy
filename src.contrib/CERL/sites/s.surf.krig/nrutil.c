#include <stdlib.h>
#include <stdio.h>

void 
nrerror (char error_text[])
{
	void exit();

	fprintf(stderr,"Numerical Recipes run-time error...\n");
	fprintf(stderr,"%s\n",error_text);
	fprintf(stderr,"...now exiting to system...\n");
	exit(1);
}

int **
imatrix (int nrl, int nrh, int ncl, int nch)
{
	int i,**m;

	m=(int **)malloc((unsigned) (nrh-nrl+1)*sizeof(int*));
	if (!m) nrerror("allocation failure 1 in imatrix()");
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(int *)malloc((unsigned) (nch-ncl+1)*sizeof(int));
		if (!m[i]) nrerror("allocation failure 2 in imatrix()");
		m[i] -= ncl;
	}
	return m;
}



float **
submatrix (float **a, int oldrl, int oldrh, int oldcl, int oldch, int newrl, int newcl)
{
	int i,j;
	float **m;

	m=(float **) malloc((unsigned) (oldrh-oldrl+1)*sizeof(float*));
	if (!m) nrerror("allocation failure in submatrix()");
	m -= newrl;

	for(i=oldrl,j=newrl;i<=oldrh;i++,j++) m[j]=a[i]+oldcl-newcl;

	return m;
}



void 
free_vector (float *v, int nl, int nh)
{
	free((char*) (v+nl));
}

void 
free_ivector (int *v, int nl, int nh)
{
	free((char*) (v+nl));
}

void 
free_dvector (double *v, int nl, int nh)
{
	free((char*) (v+nl));
}



void 
free_matrix (float **m, int nrl, int nrh, int ncl, int nch)
{
	int i;

	for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
	free((char*) (m+nrl));
}

void 
free_dmatrix (double **m, int nrl, int nrh, int ncl, int nch)
{
	int i;

	for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
	free((char*) (m+nrl));
}

void 
free_imatrix (int **m, int nrl, int nrh, int ncl, int nch)
{
	int i;

	for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
	free((char*) (m+nrl));
}



void 
free_submatrix (float **b, int nrl, int nrh, int ncl, int nch)
{
	free((char*) (b+nrl));
}



float **
convert_matrix (float *a, int nrl, int nrh, int ncl, int nch)
{
	int i,j,nrow,ncol;
	float **m;

	nrow=nrh-nrl+1;
	ncol=nch-ncl+1;
	m = (float **) malloc((unsigned) (nrow)*sizeof(float*));
	if (!m) nrerror("allocation failure in convert_matrix()");
	m -= nrl;
	for(i=0,j=nrl;i<=nrow-1;i++,j++) m[j]=a+ncol*i-ncl;
	return m;
}



void 
free_convert_matrix (float **b, int nrl, int nrh, int ncl, int nch)
{
	free((char*) (b+nrl));
}

