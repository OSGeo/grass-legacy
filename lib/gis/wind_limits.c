#include <grass/gis.h>

/* If the projection has absolute limits (like lat/lon), then
 * these routines modify the input coordinate to be within the
 * limit
 *
 * return 1 no change, 0 changed
 */

int G_limit_east ( double *east, int proj)
{
    return 1;
}

int G_limit_west (double *west, int proj)
{
    return 1;
}

int G_limit_north (double *north, int proj)
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

int G_limit_south (double *south, int proj)
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
