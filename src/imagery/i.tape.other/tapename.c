#include "tape.h"
get_tapename (name)
    char *name;
{
    for(;;)
    {
	if (!I_ask ("Enter tape device name: ", name, 1))
	    exit(0);
	if (access (name, 4) == 0)
		return;
	fprintf (stderr, "%s - ",name);
	if (access (name, 0) != 0)
	    fprintf (stderr, "no such device\n");
	else
	    fprintf (stderr, "read permission denied\n");
    }
}
