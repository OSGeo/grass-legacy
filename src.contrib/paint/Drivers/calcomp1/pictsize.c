/* %W% %G% */

#include "P.h"
int nrows, ncols, nbytes;

Ppictsize (nr, nc)
{
    nrows = nr;
    ncols = nc;
    nbytes = (nc+7)/8;

}
