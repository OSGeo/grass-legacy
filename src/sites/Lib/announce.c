#include <stdio.h>

announce (msg)
{
	fprintf(stderr,"%s",msg);
	fflush (stderr);
}
