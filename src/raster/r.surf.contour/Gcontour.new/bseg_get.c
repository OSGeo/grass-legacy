#include "gis.h"
#include "cseg.h"

bseg_get (bseg, value, row, col)
	BSEG	*bseg;
	int	row, col;
	CELL	*value;
{
	unsigned char x;
	if (segment_get (&(bseg->seg), (char *)&x, row, col>>3) < 0)
	{
		G_warning ("bseg_get(): could not read segment file at r:%d c:%d", (int) row, (int) col);
		return -1;
	}
	*value = (CELL) ((x & (1 << (col & 7))) ? 1 : 0);
	return 0;
}
