#include "defs.h"

edge_order (a, b)

	POINT *a, *b;
{
	if (a->y < b->y) return (-1);
	if (a->y > b->y) return (1);

	if (a->x < b->x) return (-1);
	if (a->x > b->x) return (1);

	return (0);
}
