#include "gis.h"

main(argc, argv) char *argv[];
{
    char name[30], *mapset;
    char result[30];
    char command[1024];

    G_gisinit (argv[0]);

    mapset = G_ask_cell_old ("Enter cell file to be resampled", name, mapset);
    if (mapset == NULL)
	exit(0);
    if (G_ask_cell_any ("Enter name for resampled cell file", result) == NULL)
	exit(0);
    
    sprintf (command, "Gresample '%s in %s' '%s'", name, mapset, result);
    system (command);
    exit(0);
}
