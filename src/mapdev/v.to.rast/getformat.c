#include "digit.h"
#include "gis.h"
#include "format.h"

getformat (Map)
    struct Map_info *Map;
{
    register i;
    CELL max, min, cat;
    unsigned char Umin, Umax;
    char Cmin, Cmax;
    short Smin, Smax;
    int first;

    max = min = 0;
    first = 1;
    for (i = 1 ; i <= Map->n_atts ; i++)
    {
	if (! ATT_ALIVE (&(Map->Att[i])))
	    continue;
	cat = Map->Att[i].cat;
	if (first)
	{
	    first = 0;
	    max = cat;
	    min = cat;
	}
	else if (cat > max)
	    max = cat;
	else if (cat < min)
	    min = cat;
    }

/* test char */
    Cmax = (char) max;
    Cmin = (char) min;
    if (Cmin == min && Cmax == max)
	return (USE_CHAR);

/* test unsigned char */
    Umax = (unsigned char) max;
    Umin = (unsigned char) min;
    if (Umin == min && Umax == max)
	return (USE_UCHAR);

/* test short */
    Smax = (short) max;
    Smin = (short) min;
    if (Smin == min && Smax == max)
	return (USE_SHORT);

    return (USE_CELL);
}
