#include "gis.h"
select_painter(name)
    char *name;
{
    char *command[1024];

    if (access(drivers(name),0) != 0)
    {
	printf ("\n*** %s - no such painter ***\n\n", name);
	return 0;
    }
    sprintf (command, "p.select %s",name);
    return (system(command) == 0);
}
