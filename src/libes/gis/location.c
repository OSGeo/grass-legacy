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
#include "glocale.h"


/*!
 * \brief current location directory
 *
 * Returns the
 * full UNIX path name of the current database location. For example, if the user
 * is working in location <i>spearfish</i> in the <i>/usr/grass5/data</i>
 * database directory, this routine will return a string which looks like
 * <i>/home/user/grassdata/spearfish</i>.
 *
 *  \param void
 *  \return char * 
 */

char *
G_location_path()
{
    char *location;

    location = G__location_path();
    if(access(location,0) != 0) 
    {
	char msg[400];

	sprintf(msg,_("LOCATION  << %s >>  not available"), location) ;
	G_fatal_error (msg);
    }

    return location;
}


/*!
 * \brief current location name
 *
 * Returns the name of
 * the current database location. This routine should be used by modules that
 * need to display the current location to the user. See Locations for
 * an explanation of locations.
 *
 *  \param void
 *  \return char * 
 */

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
