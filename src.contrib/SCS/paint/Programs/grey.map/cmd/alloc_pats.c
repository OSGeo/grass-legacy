#include "gis.h"
#include "parms.h"
allocate_pattern_array()
{
    int n,num;

    if (parms.cellfd < 0) return;
    if (parms.pattern != NULL) return;

    num = parms.max_color - parms.min_color + 1;

    parms.pattern = (PATTERN **) G_calloc(num+1, sizeof(PATTERN *));
    for (n = 0; n <= num; n++)
	parms.pattern[n] = (PATTERN *) NULL;
}
