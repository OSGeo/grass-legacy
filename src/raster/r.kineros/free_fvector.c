#include <malloc.h>
#include <stdio.h>

void free_vector(v,nl,nh)
float *v;
int nl,nh;
{
	free((char*) (v+nl));
}
