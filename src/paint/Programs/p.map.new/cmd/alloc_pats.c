#include "gis.h"
#include "parms.h"

int allocate_pattern_array (void)
{
    int n,num;

    if (parms.cellfd < 0) return 0;
    if (parms.pattern != NULL) return 0;

    num = parms.max_color - parms.min_color + 1;

    parms.pattern = (PATTERN **) G_calloc(num+1, sizeof(PATTERN *));
    for (n = 0; n <= num; n++)
	parms.pattern[n] = (PATTERN *) NULL;

    return 0;
}
