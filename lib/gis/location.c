/**********************************************************************
 *
 *   char *
 *   G_location_path()
 *
 *   returns pointer to the full path name to the current location
 *
 *   note:
 *      exits with message if path not accessible to user
 **********************************************************************
 *
 *   char *
 *   G_location()
 *
 *   returns:    pointer to string containing the one word location
 *               name.
 **********************************************************************/

#include <string.h>
#include <unistd.h>
#include "gis.h"

char *
G_location_path()
{
    char *location;

    location = G__location_path();
    if(access(location,0) != 0) 
    {
	char msg[400];

	sprintf(msg,"LOCATION  << %s >>  not available", location) ;
	G_fatal_error (msg);
    }

    return location;
}

char *
G_location()
{
    return G_getenv ("LOCATION_NAME");
}

char *
G__location_path()
{
    char *location = 0;
    char *base;
    char *name;

    name      = G_location();
    base      = G_gisdbase();
    location  = G_malloc (strlen (base) + strlen (name) + 2);
    sprintf (location, "%s/%s", base, name);

    return location;
}
