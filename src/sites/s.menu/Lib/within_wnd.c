#include "gis.h"

within_window (north, east, window)

	double north, east;
	struct Cell_head *window;
{
	return (
		north <= window->north &&
		north >= window->south &&
		east  <= window->east  &&
		east  >= window->west
	       );
}
