/***********************************************************************
 *
 *  G_remove (element, name)
 *     char *element          mapset element containing name
 *     char *name             file name to be removed
 *
 *  Only files in current mapset can be removed
 *
 *  Returns  -1  on fail
 *            0  if no file
 *            1  if successful
 *
 ***********************************************************************/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "gis.h"

int G_remove ( char *element, char *name)
{
    char path[1040];
    char *mapset;
    char xname[512], xmapset[512];
    char cmd[1100];

    if (G_legal_filename(name) < 0)
	    return -1;

/* name in mapset legal only if mapset is current mapset */
    mapset = G_mapset();
    if (G__name_is_fully_qualified (name, xname, xmapset)
    && strcmp (mapset, xmapset))
	    return -1;

/* if file does not exist, return 0 */
    if (access (G__file_name (path, element, name, mapset),0) != 0)
	    return 0;

#ifdef __MINGW32__
    if ( remove ( path ) == 0)
	    return 1;
#else
    if (unlink(path) == 0)
	    return 1;
#endif

    if (strchr(path, '\''))
	    return -1;

    sprintf(cmd, "rm -rf '%s'", path);

    if (G_system(cmd) == 0)
	    return 1;

    return -1;
}

