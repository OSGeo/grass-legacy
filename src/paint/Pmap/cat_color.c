#include "gis.h"
#include "parms.h"

cat_color_num (n)
    register int n;
{
    if (n == 0)
	return 0;
    if (n < parms.pcolr.min || n > parms.pcolr.max)
	    return -1;
    return (n - parms.pcolr.min + 1);
}
