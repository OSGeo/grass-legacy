#include "gis.h"
#include "cseg.h"

cseg_get (cseg, value, row, col)
	CSEG	*cseg;
	int	row, col;
	CELL	*value;
{
	if (segment_get (&(cseg->seg), (char *)value, row, col) < 0)
	{
		G_warning ("cseg_get(): could not read segment file");
		return -1;
	}
	return 0;
}
