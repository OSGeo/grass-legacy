#include <grass/gis.h>
#include "seg.h"

int dseg_get(DSEG * dseg, int row, int col, DCELL * value)
{
    if (segment_get(&(dseg->seg), value, row, col) < 0) {
	G_warning("dseg_get(): could not read segment file");
	return -1;
    }
    return 0;
}
