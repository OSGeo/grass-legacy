#include "P.h"
static int first = 1;
Ppictsize (nr, nc)
{
    nrows = nr;
    ncols = nc;
    if (first)
	first = 0;
    else
	formfeed();
}
