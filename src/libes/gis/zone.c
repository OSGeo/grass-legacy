/**********************************************************************
 *  G_zone()
 *
 *  Returns the zone (of the current projection)
 *  of the currently set window.
 *
 **********************************************************************/

#include "gis.h"

G_zone ()
{
    struct Cell_head window;

    G_get_set_window (&window);
    return window.zone;
}
