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

#include <grass/gis.h>
int G_remove_colr (const char *name )
{
    char secondary[GNAME_MAX + 6];

    G_remove ("colr", name);
    sprintf (secondary,"colr2/%s", G_mapset());
    G_remove (secondary, name);
    return 0;
}
