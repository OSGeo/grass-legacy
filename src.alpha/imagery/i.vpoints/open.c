#include "gis.h"

/* open and close vector file for level one access */

FILE *
open_vect(name, mapset)
    char *name, *mapset;
{
    FILE *fd ;
    char msg[128];

    fd = G_fopen_vector_old (name, mapset);
    if (fd == NULL)
    {
	sprintf (msg, "can't open vector file [%s]", name);
	G_fatal_error (msg);
    }
    if (dig_init (fd) < 0)
    {
	sprintf (msg, "can't initialize vector file [%s]", name);
	G_fatal_error (msg);
    }
    return fd;
}

close_vect(fd)
    FILE *fd;
{
    dig_fini (fd);
    fclose (fd);
}
