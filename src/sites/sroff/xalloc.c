#include <stdio.h>
char *xalloc (n)
{
	char *malloc();
	char *s;

	if (s = malloc(n))
		return (s);
	fprintf(stderr,"No memory\n");
	exit(-1);
}
