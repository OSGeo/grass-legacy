#include <malloc.h>
#include <stdio.h>

float **fmatrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
	int i;
	float **m;

	m=(float **) malloc((unsigned) (nrh-nrl)*sizeof(float*));
	if (!m) {
	    printf("\n ERROR: allocation failure 1 in fmatrix()");
	    exit(0);
	}
	m -= nrl;

	for(i=nrl;i<nrh;i++) {
		m[i]=(float *) malloc((unsigned) (nch-ncl)*sizeof(float));
		if (!m[i]) {
	    	    printf("\n ERROR: allocation failure 1 in fmatrix()");
	            exit(0);
	        }
		m[i] -= ncl;
	}
	return m;
}
