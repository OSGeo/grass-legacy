#include "gis.h"
#include "cseg.h"

cseg_put (cseg, value, row, col)
	CSEG	*cseg;
	int	row, col;
	CELL	*value;
{
	if (segment_put (&(cseg->seg), (char*) value, row, col) < 0)
	{
		G_warning ("cseg_put(): could not write segment file");
		return -1;
	}
	return 0;
}
