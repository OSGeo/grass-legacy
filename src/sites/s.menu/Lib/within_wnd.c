#include "gis.h"

within_window (north, east, window)

	struct Cell_head *window;
{
	return (
		north <= window->north &&
		north >= window->south &&
		east  <= window->east  &&
		east  >= window->west
	       );
}
