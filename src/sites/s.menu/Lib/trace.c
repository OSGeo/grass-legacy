#include <stdio.h>

trace (msg)
{
	fprintf(stderr,"%s",msg);
	fflush (stderr);
}
