/* %W% %G% */
#include "gis.h"
select_driver(name)
{
    char *drivers();
    if (access (drivers(name),0) != 0)
    {
	printf ("** PAINTER %s not found **\n", name);
	return 0;
    }
    G_setenv ("PAINTER", name);
    printf ("PAINTER %s selected\n", name);
    return 1;
}
