#include "gis.h"
select_painter(name, quiet)
    char *name;
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
	printf ("PAINTER %s selected\n", name);
    return 0;
}
