/* %W% %G% */

#include "gis.h"

main(argc,argv) char *argv[];
{
    char name[50], *mapset;
    char command[1024];
    char new[50];

    G_gisinit (argv[0]);
    mapset = G_ask_cell_old ("enter cell file to be clumped", name);
    if (mapset == NULL)
	exit(0);
    if (!G_ask_cell_new ("", new))
	exit(0);

    sprintf (command, "Gclump '%s in %s' '%s'", name, mapset, new);
    execlp ("/bin/sh", "sh", "-c", command, 0);
}
