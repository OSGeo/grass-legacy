#include "gis.h"
#include "proto.h"
#include <unistd.h>
#include <stdlib.h>

int select_painter(char *name)
{
    char command[1024];

    if (access(drivers(name),0) != 0)
    {
	fprintf (stdout,"\n*** %s - no such painter ***\n\n", name);
	return 0;
    }
    sprintf (command, "p.select %s",name);
    return (system(command) == 0);
}
