#include "gis.h"

main (argc, argv)   char *argv[];
{
    char *mapset;
    char name[100];
    char *tempfile;
    char command[512];

    G_gisinit(argv[0]);

    mapset = G_ask_old ("", name, "grid3", "3d raster");
    if (mapset == NULL)
	    exit(0);

    tempfile = G_tempfile () ;
    unlink (tempfile);

    sprintf (command, "r3.info '%s' > %s",
	G_fully_qualified_name(name, mapset), tempfile);
    system (command);

    more_print(tempfile);
    unlink (tempfile);
    return (0);
}
