#include "gis.h"
#include "cseg.h"

bseg_put (bseg, value, row, col)
	BSEG	*bseg;
	int	row, col;
	CELL	*value;
{
	unsigned char	old_value;

	if (segment_get (&(bseg->seg), (char *)&old_value, row, col>>3) < 0)
	{
		G_warning ("bseg_put(): could not read segment file");
		return -1;
	}
	if (*value)
		old_value |= (1 << (col & 7));
	else
		old_value &= ~(1 << (col & 7));
	if (segment_put (&(bseg->seg), (char *)&old_value, row, col>>3) < 0)
	{
		G_warning ("bseg_put(): could not write segment file");
		return -2;
	}
	return 0;
}
