#include <malloc.h>
#include <stdio.h>

float *fvector(nl,nh)
int nl,nh;
{
	float *v;

	v=(float *)malloc((unsigned) (nh-nl)*sizeof(float));
	if(!v) {
	    printf("\n ERROR: allocation failure in fvector()");
	    exit(0);
	}
	return v-nl;
}
