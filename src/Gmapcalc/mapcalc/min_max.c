#include "glob.h"

find_min_max (cell, ncols)
    register ncols;
    register CELL *cell;
{
    register CELL c;
    while (ncols-- > 0)
    {
	c = *cell++;
	if (c < min_value)
	    min_value = c;
	else if (c > max_value)
	    max_value = c;
    }
}
