#include <grass/gis.h>
#include "ncb.h"

/*
   given the starting col of the neighborhood,
   copy the cell values from the bufs into the array of values
   and return the number of values copied.
*/

int gather(DCELL *values, int offset)
{
    int row, col;
    int n = 0;

    *values = 0;

    for (row = 0; row < ncb.nsize; row++)
	for (col = 0; col < ncb.nsize; col++)
	{
	    DCELL *c = &ncb.buf[row][offset + col];

	    if (G_is_d_null_value(c))
		G_set_d_null_value(&values[n], 1);
	    else
		values[n] = *c;

	    n++;
	}

    return n ? n : -1;
}

