#include "gis.h"
/************************************************************
 * set_window: set the window from a data layer cell header
 ***********************************************************/

set_window (name, mapset)
    char *name;
    char *mapset;
{
    struct Cell_head cellhd;

    if (G_get_cellhd (name, mapset, &cellhd) < 0)
        die (name, mapset, "can't read header for");
    
    G_set_window (&cellhd);
}
