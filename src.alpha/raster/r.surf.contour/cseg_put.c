#include "gis.h"
#include "cseg.h"

cseg_put (cseg, row, col, value)
	CSEG	*cseg;
	int	row, col;
	CELL	value;
{
	if (segment_put (&(cseg->seg), &value, row, col) < 0)
	{
		G_warning ("cseg_put(): could not write segment file");
		return -1;
	}
	return 0;
}
