#include "gis.h"
#include "ncb.h"
CELL
c_median (values, n)
    register int n;
    CELL *values;
{
    CELL median;

/* sort the array of values, then get median */
    sort_cell (values, n, 1);
    median = (values[(n-1)/2] + values[n/2]) / 2;
    return median;
}
