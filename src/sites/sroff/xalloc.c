#include <stdio.h>
#include <stdlib.h>

char *xalloc (int n)
{
	char *s;

	if (s = malloc(n))
		return (s);
	fprintf(stderr,"No memory\n");
	exit(-1);
}
