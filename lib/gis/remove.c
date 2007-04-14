/**
 * \file remove.c
 *
 * \brief File remove functions.
 *
 * This program is free software under the GNU General Public License
 * (>=v2). Read the file COPYING that comes with GRASS for details.
 *
 * \author GRASS GIS Development Team
 *
 * \date 1999-2006
 */

#include <grass/config.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <grass/gis.h>

static int recursive_remove(const char *path);


/**
 * \fn int G_remove (char *element, char *name)
 *
 * \brief Remove a database file.
 *
 * The file or directory <b>name</b> under the database <b>element</b> directory
 * in the current mapset is removed.<br>
 * 
 * <b>Note:</b> If <b>name</b> is a directory, everything within the
 * directory is removed as well.
 *
 * \param[in] element
 * \param[in] name
 * \return 0 if <b>name</b> does not exist
 * \return 1 if successful
 * \return -1 on error
 */

int G_remove ( const char *element, const char *name)
{
    char path[1024];
    char *mapset;
    char xname[512], xmapset[512];

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

    if (recursive_remove(path) == 0)
	    return 1;

    return -1;
}


/* equivalent to rm -rf path */
static int
recursive_remove(const char *path)
{
	DIR *dirp;
	struct dirent *dp;
	struct stat sb;
	char path2[4096];

	if(G_lstat(path, &sb))
		return 1;
	if(!S_ISDIR(sb.st_mode))
		return remove(path) == 0 ? 0 : 1;

	if((dirp = opendir(path)) == NULL)
		return 1;
	while((dp = readdir(dirp)) != NULL)
	{
		if(dp->d_name[0] == '.')
			continue;
		if (strlen(path) + strlen(dp->d_name) + 2 > sizeof(path2))
			continue;
		sprintf(path2, "%s/%s", path, dp->d_name);
		recursive_remove(path2);
	}
	closedir(dirp);

	return rmdir(path) == 0 ? 0 : 1;
}
