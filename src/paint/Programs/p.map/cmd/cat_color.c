#include "gis.h"
#include "parms.h"

int 
cat_color_num (register int n)
{
    if (n == 0)
	return 0;
    if (n < parms.min_color || n > parms.max_color)
	    return -1;
    return (n - parms.min_color + 1);
}
