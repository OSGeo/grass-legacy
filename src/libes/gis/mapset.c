/**********************************************************************
 *
 *   char *
 *   G_mapset()
 *
 *   returns:    pointer to string containing the one word mapset
 *               name.
 *               NULL if user does not have access to mapset.
 *
 **********************************************************************/

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "glocale.h"


/*!
 * \brief current mapset name
 *
 * Returns the name of the
 * current mapset in the current location. This routine is often used when
 * accessing files in the current mapset. See Mapsets for an
 * explanation of mapsets.
 *
 *  \param void
 *  \return char * 
 */

char *
G_mapset()
{
    static char mapset[30];
    static int first = 1;
    char *m;

    char msg[100];

    m = G__mapset();
    if( m == NULL )
        G_fatal_error( _("MAPSET is not set") );

    if (first)
	    first = 0;
    else if (strcmp(mapset,m) == 0)
	    return mapset;
    strcpy (mapset,m);

    switch (G__mapset_permissions (mapset))
    {
    case 0:
    case 1:
	    return mapset;
    /*
    case 0:
	    sprintf(msg,"MAPSET %s - permission denied", mapset);
	    break;
    */
    default:
	    sprintf(msg,_("MAPSET %s not found"), mapset);
	    break;
    }
    G_fatal_error (msg);
    exit(-1);
}

char *
G__mapset()
{
    return G__getenv("MAPSET");
}

