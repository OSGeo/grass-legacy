#include "gis.h"

select_painter(name, quiet)
char *name;
int quiet;
{
    char *ps_devices();

    if (access (ps_devices(name), 0) != 0)
    {
	G_unsetenv("PSPAINTER");
	fprintf(stderr, "** POSTSCRIPT PAINTER %s not found **\n", name);
	return 1;
    }
    G_setenv("PSPAINTER", name);
    if (!quiet) printf("POSTSCRIPT PAINTER %s selected\n", name);
    return 0;
}
