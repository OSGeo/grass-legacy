#include "glob.h"

/* round(x) rounds x to nearest CELL value, handles negative correctly */
CELL
round (x)
    double x;
{
    CELL n;

    if (x > HUGE || x < -HUGE)
    {
	n = 0;
	overflow_occurred = 1;
    }
    else if (x >= 0.0)
	n = x + .5;
    else
    {
	n = -x+.5;
	n = -n;
    }
    return n;
}
