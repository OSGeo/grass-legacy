#include "defs.h"

int 
edge_order (POINT *a, POINT *b)
{
	if (a->y < b->y) return (-1);
	if (a->y > b->y) return (1);

	if (a->x < b->x) return (-1);
	if (a->x > b->x) return (1);

	return (0);
}
