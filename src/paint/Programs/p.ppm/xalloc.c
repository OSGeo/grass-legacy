#include <stdio.h>

extern char *pgm;

char *
xalloc (int n)
{
    char *malloc();
    char *b;

    if (b = malloc(n))
	return b;
    
    fprintf (stderr, "%s - out of memory\n", pgm);
    exit(1);
}
