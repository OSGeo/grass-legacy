#include <stdio.h>

announce (msg)
	char *msg;
{
	fprintf(stderr,"%s",msg);
	fflush (stderr);
}
