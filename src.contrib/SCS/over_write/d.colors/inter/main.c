#include "gis.h"
main(argc, argv) char *argv[];
{
    char name[512];
    char *mapset;

    G_gisinit(argv[0]);
    mapset = G_ask_cell_old("",name);
    if  (mapset == NULL) exit(0);

    strcat (name, " in ");
    strcat (name, mapset);

    execlp ("d.colors", "d.colors", name, (char *) 0);
    fprintf (stderr, "ERROR: unable to run d.colors\n");
    exit(1);
}
