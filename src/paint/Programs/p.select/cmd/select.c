#include "gis.h"
#include <unistd.h>
int 
select_painter (char *name, int quiet)
{
    char *drivers();
    if (access (drivers(name),0) != 0)
    {
	G_unsetenv("PAINTER");
	fprintf (stderr, "** PAINTER %s not found **\n", name);
	return 1;
    }
    G_setenv ("PAINTER", name);
    if (!quiet)
	fprintf (stdout,"PAINTER %s selected\n", name);
    return 0;
}
