#include <stdio.h>

int 
die (char *msg)
{
	fprintf(stderr,"%s\n", msg);
	exit(-1);
}
