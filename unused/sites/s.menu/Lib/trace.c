#include <stdio.h>

int trace (int msg)
{
	fprintf(stderr,"%s",msg);
	fflush (stderr);
}
