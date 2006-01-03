#include <stdlib.h>
#include "gis.h"

int 
open_cell_old (char *name, char *mapset)
{
    int fd;

    if (mapset == NULL) mapset = G_find_cell (name, "");
    fd = G_open_cell_old (name, mapset);
    if (fd >= 0)
	return fd;
    
    fprintf (stderr, "ERROR: unable to open raster map [%s]\n", name);
    exit(1);
}

int 
open_cell_new (char *name)
{
    int fd;

    fd = G_open_cell_new (name);
    if (fd >= 0)
	return fd;

    fprintf (stderr, "ERROR: unable to create raster map [%s]\n", name);
    exit(1);
}
