#include <grass/gis.h>
#include "seg.h"

int dseg_put(DSEG * dseg, int row, int col, DCELL value)
{
    if (segment_put(&(dseg->seg), &value, row, col) < 0) {
	G_warning("dseg_put(): could not write segment file");
	return -1;
    }
    return 0;
}
