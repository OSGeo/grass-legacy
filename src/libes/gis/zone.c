/**********************************************************************
 *  G_zone()
 *
 *  Returns the zone (of the current projection)
 *  of the currently set window.
 *
 **********************************************************************/

#include "gis.h"

/*!
 * \brief query cartographic zone
 *
 * This routine returns the
 * zone for the active region. The meaning for the zone depends on the
 * projection. For example zone 18 for projection type 1 would be UTM zone 18.
 *
 *  \param void
 *  \return int
 */

int G_zone ()
{
    struct Cell_head window;

    G_get_set_window (&window);
    return window.zone;
}
