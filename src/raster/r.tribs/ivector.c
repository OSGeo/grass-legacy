#include <malloc.h>
#include <stdio.h>

int *ivector(nl,nh)
int nl,nh;
{
    int *v;

    v=(int *)malloc((unsigned) (nh-nl)*sizeof(int));
    if (!v) {
	printf("\n ERROR: allocation failure in ivector()");
	exit(0);
    }
    return v-nl;
}
