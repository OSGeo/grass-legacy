
#include <stdlib.h>
#include "gis.h"

static int ascending(const void *aa, const void *bb)
{
	const DCELL *a = aa, *b = bb;
	if (G_is_d_null_value((DCELL *) a)) return 1;
	if (G_is_d_null_value((DCELL *) b)) return -1;
	return (*a < *b) ? -1 : (*a > *b) ? 1 : 0;
}

void sort_cell(DCELL *array, int n)
{
	qsort(array, n, sizeof(DCELL), ascending);
}

