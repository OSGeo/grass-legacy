/* %W% %G% */
/**********************************************************************
 *  G_zone()
 *
 *  Returns the zone (of the current projection) of the currently set window.
 *
 **********************************************************************
 *  G_projection()
 *
 *  Returns the projection type of the currently set window.
 *  (Note this is really the coordinate system, not the projection)
 *       0 - x,y (Raw imagery)
 *       1 - UTM   Universal Transverse Mercator
 *       2 - State Plane (in feet)
 *
 **********************************************************************
 *
 * char *
 * G_projection_name (n)
 *
 * Returns printable name for projection type n
 * return NULL if not known
 *
 *
 **********************************************************************
 *
 * G_projection_units (n)
 *
 * Returns DEFINED units for projection type n
 *
 * For projection 0, returns 0
 * For projection 1, returns METERS
 * For projection 2, returns FEET
 * returns -1 if other
 *********************************************************************/

#include "gis.h"

G_zone ()
{
    struct Cell_head window;

    G_get_set_window (&window);
    return window.zone;
}

G_projection ()
{
    struct Cell_head window;

    G_get_set_window (&window);
    return window.proj;
}

char *
G_projection_name(n)
{
    switch (n)
    {
    case 0:
	return "x,y";
    case 1:
	return "UTM";
    case 2:
	return "State Plane";
    default:
	return NULL;
    }
}

G_projection_units(n)
{
    switch (n)
    {
    case 0:
	return 0;
    case 1:
	return METERS;
    case 2:
	return FEET;
    default:
	return -1;
    }
}

char *
G_unit_name (unit, plural)
{
    switch (unit)
    {
    case 0:
	return plural ? "units" : "unit";
    case METERS:
	return plural ? "meters" : "meter";
    case FEET:
	return plural ? "feet" : "foot";
    default:
	return NULL;
    }
}
