/*
   these routines returns row, col relative to the window 
*/

#include "gis.h"

float 
northing_to_row (double north, struct Cell_head *window)
{
	return (float)G_northing_to_row(north, window);
}

float 
easting_to_col (double east, struct Cell_head *window)
{
	return (float)G_easting_to_col(east, window);
}

