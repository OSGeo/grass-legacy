#include <stdio.h>
#include "config.h"

int process_old (FILE *in, FILE *out, char *file)
{
    if (!in)
	return 1;

    while (gather(in))
    {
	setup (file,0);
	if(!modify())
	    return 0;
	update(out);
    }
    return 1;
}
