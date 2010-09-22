#include <grass/gis.h>
#include "seg.h"

int bseg_get(BSEG * bseg, char * value, int row, int col)
{
    unsigned char x;
    char errmsg[200];

    if (segment_get(&(bseg->seg), &x, row, col >> 3) < 0) {
	sprintf(errmsg,
		"bseg_get(): could not read segment file at r:%d c:%d",
		(int)row, (int)col);
	G_warning(errmsg);
	return -1;
    }
    *value = (char) ((x & (1 << (col & 7))) ? 1 : 0);
    return 0;
}
