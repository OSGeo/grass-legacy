#include <stdio.h>
#include "interface.h"
Pconnect()
{
    char *getenv();
    char *device;
    char transparent;

    device = getenv ("PAINT_DRIVER_SHELL");
    if (device == NULL)
    {
	fprintf (stderr, "PAINT_DRIVER_SHELL not set\n");
	exit(1);
    }
    P__opendev (device);
    P__readdev (&transparent, 1);
    P__transparent (transparent);
}
Pdisconnect()
{
    P__closedev();
}
