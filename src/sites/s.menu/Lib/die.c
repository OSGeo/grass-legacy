#include <stdio.h>

die (msg)	char *msg;
{
	fprintf(stderr,"%s\n", msg);
	exit(-1);
}
