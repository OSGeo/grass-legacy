#include "gis.h"
#include "cseg.h"

seg_put (sseg, value, row, col)
	SSEG	*sseg;
	int	row, col;
	char	*value;
{
	if (segment_put (&(sseg->seg), value, row, col) < 0)
	{
		G_warning ("seg_put(): could not write segment file");
		return -1;
	}
	return 0;
}
