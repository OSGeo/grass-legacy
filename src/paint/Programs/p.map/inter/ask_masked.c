#include <stdio.h>
#include "local_proto.h"

extern int masking_on;

int 
ask_masked (char *name, FILE *fd)
{
    if (masking_on)
    {
	fprintf (stdout,"should %s be masked by the current mask", name);
	if (yes(""))
	    fprintf (fd, "  masked\n");
    }

  return 0;
}

