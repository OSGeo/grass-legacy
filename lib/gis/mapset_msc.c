/*********************************************************
 * G__make_mapset_element (element)
 *     char *element           element to be created in mapset
 *
 * make the specified element in the current mapset
 * will check for the existence of the element and
 * do nothing if it is found so this routine
 * can be called even if the element already exists
 ********************************************************/

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grass/gis.h>
#include <grass/glocale.h>

int G__make_mapset_element (const char *p_element)
{
    char path[GPATH_MAX];
    char *p;
    const char *element;

    element = p_element;
    if (*element == 0)
	    return 0;

    G__file_name (p = path, "", "", G_mapset());
    while (*p)
	p++;
/* add trailing slash if missing */
    --p;
    if (*p++ != '/')
    {
	*p++ = '/' ;
	*p = 0;
    }

/* now append element, one directory at a time, to path */
    while (1)
    {
	if (*element == '/' || *element == 0)
	{
	    *p = 0;
	    if (access (path, 0) != 0)
		G_mkdir(path);
	    if (access (path, 0) != 0)
		G_fatal_error (_("can't make mapset element %s (%s)"), p_element, path);
	    if (*element == 0)
		return 1;
	}
	*p++ = *element++;
    }
}

int G__make_mapset_element_misc (const char *dir, const char *name)
{
    char buf[GNAME_MAX*2+1];

    sprintf(buf, "%s/%s", dir, name);
    return G__make_mapset_element(buf);
}

/****************************************************************
* G__mapset_permissions (mapset)
*
* returns: 1 mapset exists, and user has permission
*          0 mapset exists, BUT user denied permission
*         -1 mapset does not exist
****************************************************************/
int G__mapset_permissions (const char *mapset)
{
    char path[GPATH_MAX];
    struct stat info;

    G__file_name (path,"","",mapset);

    if (G_stat (path, &info) != 0)
	    return -1;
    if (!S_ISDIR(info.st_mode))
	    return -1;

#ifndef __MINGW32__    
    if (info.st_uid != getuid())
	    return 0;
    if (info.st_uid != geteuid())
	    return 0;
#endif
    
    return 1;
}

/****************************************************************
* G__mapset_permissions2 ( gisdbase, location, mapset)
*
* mapset_path is full path to mapset directory
*
* returns: 1 mapset exists, and user has permission
*          0 mapset exists, BUT user denied permission
*         -1 mapset does not exist
****************************************************************/
int G__mapset_permissions2 ( const char * gisdbase, const char * location, const char *mapset )
{
    char path[GPATH_MAX];
    struct stat info;

    sprintf ( path, "%s/%s/%s", gisdbase, location, mapset );

    if (G_stat (path, &info) != 0)
	    return -1;
    if (!S_ISDIR(info.st_mode))
	    return -1;

#ifndef __MINGW32__    
    if (info.st_uid != getuid())
	    return 0;
    if (info.st_uid != geteuid())
	    return 0;
#endif
    
    return 1;
}

