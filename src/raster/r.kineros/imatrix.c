#include <malloc.h>
#include <stdio.h>

int **imatrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
    int i,**m;

    m=(int **)malloc((unsigned) (nrh-nrl)*sizeof(int*));
    if (!m) {
	printf("\n ERROR: allocation failure 1 in imatrix()");
	exit(0);
    }
    m -= nrl;

    for(i=nrl;i<nrh;i++) {
	m[i]=(int *)malloc((unsigned) (nch-ncl)*sizeof(int));
	if (!m[i]) {
	    printf("\n ERROR: allocation failure 1 in imatrix()");
	    exit(0);
        }
	m[i] -= ncl;
    }

    printf("\n MALLOC of %d rows and %d columns successful",(nrh-nrl),(nch-ncl));
    return m;
}
