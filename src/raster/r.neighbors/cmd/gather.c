#include "gis.h"
#include "ncb.h"
/*
   given the starting col of the neighborhood,
   copy the categories from the bufs into the array of values
   and return the number of values copied.
   note: category zero is not copied or counted
	 unless all values are 0
*/

gather (values, offset)
    CELL *values;
{
    int n;
    int row;
    int col;
    CELL *c;

    *values = 0;
    n = 0;

    for (row = 0; row < ncb.nsize; row++)
    {
	c = ncb.buf[row] + offset; 
	for (col = 0; col < ncb.nsize; col++, c++)
	    if (*c)
	    {
		*values++ = *c;
		n++;
	    }
    }
    if (n == 0) n = 1;

    return n;
}
