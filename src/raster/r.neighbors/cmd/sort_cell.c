/* sort an array of CELL */
#include "gis.h"

sort_cell (array, n, order)
    CELL array[];
{
    int ascending();
    int descending();

    if (order > 0)
	qsort (array, n, sizeof(CELL), ascending);
    else
	qsort (array, n, sizeof(CELL), descending);
}
static ascending (a ,b)
    CELL *a, *b;
{
    return (*a - *b) ;
}
static descending (a ,b)
    CELL *a, *b;
{
    return (*b - *a) ;
}
