#include <unistd.h>
#include <stdio.h>
#include "imagery.h"

int get_tapename (char *name)
{
    for(;;)
    {
	if (!I_ask ("enter tape device name: ", name, 1))
	    exit(0);
	if (access (name, 4) == 0)
		return 0;
	fprintf (stderr, "%s - ",name);
	if (access (name, 0) != 0)
	    fprintf (stderr, "no such device\n");
	else
	    fprintf (stderr, "read permission denied\n");
    }

    return 0;
}
