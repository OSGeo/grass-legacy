#include "P.h"
Ppictsize (rows, cols)
{
    if (cols > window_ncols)
	error ("Picture size is too large for the graphics frame");

    erase();
}
