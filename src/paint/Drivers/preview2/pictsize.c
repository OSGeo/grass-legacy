#include "P.h"
#include "Paintlib.h"
#include "local_proto.h"

int Ppictsize (int rows, int cols)
{
    if (cols > window_ncols)
	paint_error ("Picture size is too large for the graphics frame");

    erase();

    return 0;
}
