/**********************************************************************
 *  G_zone()
 *
 *  Returns the zone (of the current projection)
 *  of the currently set window.
 *
 **********************************************************************
 *  G_projection()
 *
 *  Returns the projection type of the currently set window.
 *  (Note this is really the coordinate system, not the projection)
 *    PROJECTION_XY  0 - x,y (Raw imagery)
 *    PROJECTION_UTM 1 - UTM   Universal Transverse Mercator
 *    PROJECTION_SP  2 - State Plane (in feet)
 *    PROJECTION_LL  3 - Latitude-Longitude
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
 * For PROJECTION_XY,  returns 0
 * For PROJECTION_UTM, returns METERS
 * For PROJECTION_SP,  returns FEET
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
    case PROJECTION_XY:
	return "x,y";
    case PROJECTION_UTM:
	return "UTM";
    case PROJECTION_SP:
	return "State Plane";
    case PROJECTION_LL:
	return "Latitude-Longitude";
    default:
	return NULL;
    }
}

G_projection_units(n)
{
    switch (n)
    {
    case PROJECTION_XY:
	return 0;
    case PROJECTION_UTM:
	return METERS;
    case PROJECTION_SP:
	return FEET;
    case PROJECTION_LL:
	return DEGREES;
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
    case DEGREES:
	return plural ? "degrees" : "degree";
    default:
	return NULL;
    }
}
