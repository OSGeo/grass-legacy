/* sort an array of CELL */
#include <stdlib.h>
#include "gis.h"

static int ascending (DCELL *,DCELL *);
static int descending (DCELL *,DCELL *);

int sort_cell (DCELL array[],int n, int order)
{
    if (order > 0)
	qsort (array, n, sizeof(DCELL), ascending);
    else
	qsort (array, n, sizeof(DCELL), descending);

    return 0;
}
static int ascending (DCELL *a,DCELL *b)
{
    if(G_is_d_null_value(a)) return 1;
    if(G_is_d_null_value(b)) return -1;
    return (*a - *b) ;
}
static int descending (DCELL *a,DCELL *b)
{
    if(G_is_d_null_value(a)) return -1;
    if(G_is_d_null_value(b)) return 1;
    return (*b - *a) ;
}
