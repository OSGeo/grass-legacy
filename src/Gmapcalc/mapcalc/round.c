#include "gis.h"

/* round(x) rounds x to nearest CELL value, handles negative correctly */
CELL
round (x)
    double x;
{
    CELL n;

    if (x >= 0.0)
	n = x + .5;
    else
    {
	n = -x+.5;
	n = -n;
    }
    return n;
}
