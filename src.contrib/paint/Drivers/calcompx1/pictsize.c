/* %W% %G% */

#include "P.h"
int nrows, ncols, nbytes;
int pict_row;

Ppictsize (nr, nc)
{
    nrows = nr;
    ncols = nc;
    nbytes = (nc+7)/8;

    pict_row = 0;
}
