#include "gis.h"
#include "3d.h"
#include "options.h"
#include <stdlib.h>

int 
check_options (void)
{
	int delta_north, delta_east ;

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

	switch (direction)
	{
	case WEST_NORTH:
	case NORTH_WEST:
		if (window.west < from_easting + 2 * window.ew_res)
			window.west = from_easting + 2 * window.ew_res ;
		if (window.north > from_northing - 2 * window.ns_res)
			window.north = from_northing - 2 * window.ns_res ;
		break ;
	case NORTH_EAST:
	case EAST_NORTH:
		if (window.east > from_easting - 2 * window.ew_res)
			window.east = from_easting - 2 * window.ew_res ;
		if (window.north > from_northing - 2 * window.ns_res)
			window.north = from_northing - 2 * window.ns_res ;
		break ;
	case EAST_SOUTH:
	case SOUTH_EAST:
		if (window.east > from_easting - 2 * window.ew_res)
			window.east = from_easting - 2 * window.ew_res ;
		if (window.south < from_northing + 2 * window.ns_res)
			window.south = from_northing + 2 * window.ns_res ;
		break ;
	case SOUTH_WEST:
	case WEST_SOUTH:
		if (window.west < from_easting + 2 * window.ew_res)
			window.west = from_easting + 2 * window.ew_res ;
		if (window.south < from_northing + 2 * window.ns_res)
			window.south = from_northing + 2 * window.ns_res ;
		break ;
	}

	return(0) ;
}
