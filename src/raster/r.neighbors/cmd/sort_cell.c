/* sort an array of CELL */
#include <stdlib.h>
#include "gis.h"

static int ascending (const void *, const void *);
static int descending (const void *, const void *);

int sort_cell (DCELL array[],int n, int order)
{
    if (order > 0)
	qsort (array, n, sizeof(DCELL), ascending);
    else
	qsort (array, n, sizeof(DCELL), descending);

    return 0;
}
static int ascending (const void *aa, const void *bb)
{
    const DCELL *a = aa, *b = bb;
    if(G_is_d_null_value((DCELL *) a)) return 1;
    if(G_is_d_null_value((DCELL *) b)) return -1;
    return (*a < *b) ? -1 : (*a > *b) ? 1 : 0;
}
static int descending (const void *aa, const void *bb)
{
    const DCELL *a = aa, *b = bb;
    if(G_is_d_null_value((DCELL *) a)) return -1;
    if(G_is_d_null_value((DCELL *) b)) return 1;
    return (*b < *a) ? -1 : (*b > *a) ? 1 : 0;
}
