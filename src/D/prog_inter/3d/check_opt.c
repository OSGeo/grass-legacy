/* @(#)check_opt.c	2.1   10/1/87 */
#include "gis.h"
#include "3d.h"
#include "options.h"

check_options()
{
    int delta_north, delta_east ;
    int err ;
    char *mapset ;

    delta_north = from_northing - to_northing ;
    delta_east  = from_easting  - to_easting  ;

    if (abs(delta_north) > abs(delta_east))
    {
	if (delta_north > 0)
	{
	    if (delta_east > 0)
		direction = NORTH_EAST ;
	    else
		direction = NORTH_WEST ;
	}
	else
	{
	    if (delta_east > 0)
		direction = SOUTH_EAST ;
	    else
		direction = SOUTH_WEST ;
	}
    }
    else
    {
	if (delta_east > 0)
	{
	    if (delta_north > 0)
		direction = EAST_NORTH ;
	    else
		direction = EAST_SOUTH ;
	}
	else
	{
	    if (delta_north > 0)
		direction = WEST_NORTH ;
	    else
		direction = WEST_SOUTH ;
	}
    }

    return(0) ;
}
