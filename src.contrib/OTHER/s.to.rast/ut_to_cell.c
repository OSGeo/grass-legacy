/*
   these routines returns row, col relative to the window 
*/

#include "gis.h"

float
northing_to_row (north, window)

	struct Cell_head *window;
{
	return (float)((window->north - north)/window->ns_res);
}

float
easting_to_col (east, window)

	struct Cell_head *window;
{
	return (float)((east - window->west)/window->ew_res) ;
}
