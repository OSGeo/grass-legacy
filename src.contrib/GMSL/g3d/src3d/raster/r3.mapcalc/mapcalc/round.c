#include "glob.h"

/* round(x) rounds x to nearest int value, handles negative correctly */
int
round (x)
    double x;
{
    int n;

    if (x >= 0.0)
	n = x + .5;
    else
    {
	n = -x+.5;
	n = -n;
    }
    return n;
}
