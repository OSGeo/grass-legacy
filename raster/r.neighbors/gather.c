#include "gis.h"
#include "ncb.h"
/*
   given the starting col of the neighborhood,
   copy the cell values from the bufs into the array of values
   and return the number of values copied.
   note: n-data cells are not copied or counted
	 unless all values are np-data
*/

int gather (DCELL *values,int offset)
{
    int n;
    int row;
    int col;
    DCELL *c;

    *values = 0;
    n = 0;

    for (row = 0; row < ncb.nsize; row++)
    {
	c = ncb.buf[row] + offset; 
	for (col = 0; col < ncb.nsize; col++, c++)
	{
	    if (G_is_d_null_value(c))
		return -1;
	    *values++ = *c;
	    n++;
	}
    }
    if (n == 0) n = 1;

    return n;
}
