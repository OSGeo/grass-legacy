#include "gis.h"
#include "version.h"
#include <stdlib.h>
#include <string.h>

/*!
 *  \brief reports GRASS version
 *  \param major 
 *  \param minor
 *  \param release
 *  \return int
 */

int
G_version (char *major, char *minor, char *release)
{
    int len;

    len = strlen(GRASS_VERSION_MAJOR);
    major = G_malloc (len);
    sprintf(major, "%s", GRASS_VERSION_MAJOR);
    
    len = strlen(GRASS_VERSION_MINOR);
    minor = G_malloc (len);
    sprintf(minor, "%s", GRASS_VERSION_MINOR);
    
    len = strlen(GRASS_VERSION_RELEASE);
    release = G_malloc (len);
    sprintf(release, "%s", GRASS_VERSION_RELEASE);

    return 0;    
}
