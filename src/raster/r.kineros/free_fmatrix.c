#include <malloc.h>
#include <stdio.h>

void free_fmatrix(m,nrl,nrh,ncl,nch)
float **m;
int nrl,nrh,ncl,nch;
{
	int i;

    for(i=nrh-1;i>=nrl;i--) 
	free((char*) (m[i]+ncl));
    free((char*) (m+nrl));
}
