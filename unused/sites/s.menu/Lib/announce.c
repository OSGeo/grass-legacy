#include <stdio.h>

int announce (char *msg)
{
	fprintf(stderr,"%s",msg);
	fflush (stderr);

	return 0;
}
