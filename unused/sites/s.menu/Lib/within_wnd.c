#include "gis.h"

int within_window (double north, double east, struct Cell_head *window)
{
	return (
		north <= window->north &&
		north >= window->south &&
		east  <= window->east  &&
		east  >= window->west
	       );
}
