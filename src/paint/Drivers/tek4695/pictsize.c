#include "P.h"
static int first = 1;
Ppictsize (nr, nc)
{
    nrows = nr;
    ncols = nc;
    nbytes = (nc+7)/8;
    if (first)
	first = 0;
    else
	formfeed();
}
