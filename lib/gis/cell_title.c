/**************************************************************
 * char *G_get_cell_title (name, mapset)
 *   char *name        name of map file
 *   char *mapset      mapset containing name
 *
 *   returns pointer to string containing cell title. (from cats file)
 *************************************************************/

#include "gis.h"

char *
G_get_cell_title (name, mapset)
    char *name;
    char *mapset;
{
    FILE *fd;
    int stat;
    char title[1024];

    stat = -1;
    fd = G_fopen_old ("cats", name, mapset);
    if (fd)
    {
	stat = 1;
	if (!fgets(title, sizeof title, fd))   /* skip number of cats */
	    stat = -1;
	else if (!G_getl(title, sizeof title, fd))      /* read title */
	    stat = -1;

	fclose (fd);
    }

    if (stat < 0)
	*title = 0;
    else
	G_strip (title);
    return G_store(title) ;
}
