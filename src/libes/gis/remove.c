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

#include "gis.h"
G_remove (element, name)
    char *element;
    char *name;
{
    char *path;
    char command[1040];
    char *mapset;
    char tname[256], tmap[256];

/* name in mapset legal only if mapset is current mapset */
    mapset = G_mapset();
    if (G__name_in_mapset (name, tname, tmap) && strcmp (mapset, tmap))
	    return -1;

    strcpy (command, "rm -rf ");
    path = command + strlen (command);
/* if file does not exist, return 0 */
    if (access (G__file_name (path, element, name, mapset),0) != 0)
	    return 0;

/* otherwise return result from rm */
    return system (command) == 0 ? 1 : -1;
}
