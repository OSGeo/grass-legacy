/**********************************************************************
 *  char *
 *  G__file_name (path, element, name, maps)
 *      char path[]       buffer to hold resultant full path to file.
 *      char *element     database element (eg, "cell", "cellhd", etc)
 *      char *name        name of file to build path to
 *      char *maps        mapset name
 *   
 *      builds full path names to GIS data files
 *
 *  returns:
 *         pointer to 'path'
 *
 *   note:
 *      if name is of the form nnn@ppp then path is set
 *      as if name had been nnn and mapset had been ppp
 *      (mapset parameter itself is ignored in this case)
 *********************************************************************/

#include <string.h>
#include "gis.h"

char *G__file_name ( 
	char *path,
	char *element,
	char *name,
	char *mapset)
{
	char xname[512];
	char xmapset[512];

/*
 * if a name is given, build a file name
 * must split the name into name, mapset if it is
 * in the name@mapset format
 */
	if (name && *name && G__name_is_fully_qualified(name, xname, xmapset))
	{
		strcpy(name, xname);
		sprintf(path,"%s/%s", G__location_path(), xmapset);
	}
	else
	if (mapset && *mapset)
		sprintf(path,"%s/%s", G__location_path(), mapset);
	else
		sprintf(path,"%s/%s", G__location_path(), G_mapset());

	if (element && *element)
	{
		strcat (path, "/");
		strcat (path, element);
	}

	if (name && *name)
	{
		strcat (path, "/");
		strcat (path, name);
	}

/*
 * return pointer to users 'path' buffer
 */
	return path;
}
