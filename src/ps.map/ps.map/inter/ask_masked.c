#include <stdio.h>

extern int masking_on;

ask_masked (name,fd)
    char *name;
    FILE *fd;
{
    if (masking_on)
    {
	printf ("should %s be masked by the current mask", name);
	if (yes(""))
	    fprintf (fd, "  masked\n");
    }
}

