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
#include "gis.h"
#include "glocale.h"

#include <sys/types.h>
#include <sys/stat.h>

int G__make_mapset_element (char *p_element)
{
    char command[1024];
    char *path;
    char *p;
    char *G_mapset();
    char *element;

    element = p_element;
    if (*element == 0)
	    return 0;
    strcpy (path = command, "mkdir ");
    while (*path)
	path++;

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
/* MOD shapiro 16apr91 */
	    if (access (path, 0) != 0)
	    {
#ifdef __MINGW32__
		mkdir(path);
#else                
		mkdir(path,0777);
#endif
	}
/* end MOD */
	    if (access (path, 0) != 0)
	    {
		system (command);
	    }
	    if (access (path, 0) != 0)
	    {
		char err[1024];
		sprintf (err, _("can't make mapset element %s (%s)"), p_element, path);
		G_fatal_error (err);
		exit(1);
	    }
	    if (*element == 0)
		return 1;
	}
	*p++ = *element++;
    }
}

/****************************************************************
* G__mapset_permissions (mapset)
*
* returns: 1 mapset exists, and user has permission
*          0 mapset exists, BUT user denied permission
*         -1 mapset does not exist
****************************************************************/
int G__mapset_permissions (char *mapset)
{
    char path[2000];
    struct stat info;

    G__file_name (path,"","",mapset);

    if (stat (path, &info) != 0)
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
int G__mapset_permissions2 ( char * gisdbase, char * location, char *mapset )
{
    char path[2000];
    struct stat info;

    sprintf ( path, "%s/%s/%s", gisdbase, location, mapset );

    if (stat (path, &info) != 0)
	    return -1;

#ifndef __MINGW32__    
    if (info.st_uid != getuid())
	    return 0;
    if (info.st_uid != geteuid())
	    return 0;
#endif
    
    return 1;
}

