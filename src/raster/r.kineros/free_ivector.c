#include <malloc.h>
#include <stdio.h>

void free_ivector(v,nl,nh)
int *v,nl,nh;
{
	free((char*) (v+nl));
}
