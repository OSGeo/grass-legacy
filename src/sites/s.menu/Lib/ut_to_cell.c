/*
   these routines returns row, col relative to the window 
*/

#include "gis.h"

float
northing_to_row (north, window)

	struct Cell_head *window;
        double north;
{
	return (float)G_northing_to_row(north, window);
}

float
easting_to_col (east, window)

	struct Cell_head *window;
        double east;
{
	return (float)G_easting_to_col(east, window);
}

