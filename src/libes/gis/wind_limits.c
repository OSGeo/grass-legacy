#include "gis.h"

/* If the projection has absolute limits (like lat/lon), then
 * these routines modify the input coordinate to be within the
 * limit
 *
 * return 1 no change, 0 changed
 */

G_limit_east (east, proj)
    double *east;
{
    return 1;
}

G_limit_west (west, proj)
    double *west;
{
    return 1;
}

G_limit_north (north, proj)
    double *north;
{
    if (proj == PROJECTION_LL)
    {
	if(*north > 90.0)
	{
	    *north = 90.0;
	    return 0;
	}
	if (*north < -90)
	{
	    *north = -90;
	    return 0;
	}
    }
    return 1;
}

G_limit_south (south, proj)
    double *south;
{
    if (proj == PROJECTION_LL)
    {
	if(*south > 90.0)
	{
	    *south = 90.0;
	    return 0;
	}
	if (*south < -90)
	{
	    *south = -90;
	    return 0;
	}
    }
    return 1;
}
