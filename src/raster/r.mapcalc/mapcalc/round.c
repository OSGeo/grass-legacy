#include "glob.h"

/* round(x) rounds x to nearest CELL value, handles negative correctly */
CELL 
round (double x)
{
    CELL n;

    if (ISNULL_D(&x) || x > HUGE || x < -HUGE)
    {
	SETNULL(&n);
	if (!ISNULL_D(&x))
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
