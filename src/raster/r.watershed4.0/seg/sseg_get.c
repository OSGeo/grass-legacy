#include "gis.h"
#include "cseg.h"

seg_get (sseg, value, row, col)
	SSEG	*sseg;
	char	*value;
	int	row, col;
{
	if (segment_get (&(sseg->seg), value, row, col) < 0)
	{
		G_warning ("seg_get(): could not read segment file");
		return -1;
	}
	return 0;
}
