/**********************************************************************
 *
 *  G_remove_colr (name)
 *      char *name                   name of map to remove color table
 *
 *  Removes the color information associated with map layer "map"
 *   (in the current mapset)
 *
 *  returns:    0
 *********************************************************************/

#include "gis.h"
int G_remove_colr (char *name )
{
    char secondary[50];

    G_remove ("colr", name);
    sprintf (secondary,"colr2/%s", G_mapset());
    G_remove (secondary, name);
    return 0;
}
