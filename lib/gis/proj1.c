/**********************************************************************
 *  G_projection()
 *
 *  Returns the projection type of the currently set window.
 *  (Note this is really the coordinate system, not the projection)
 *    PROJECTION_XY  0 - x,y (Raw imagery)
 *    PROJECTION_UTM 1 - UTM   Universal Transverse Mercator
 *    PROJECTION_SP  2 - State Plane (in feet)
 *    PROJECTION_LL  3 - Latitude-Longitude
 *
 **********************************************************************/

#include "gis.h"

int G_projection ()
{
    struct Cell_head window;

    G_get_set_window (&window);
    return window.proj;
}
