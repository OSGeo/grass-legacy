#include "gis.h"
#include "cseg.h"

dseg_get (dseg, value, row, col)
	DSEG	*dseg;
	int	row, col;
	double	*value;
{
	if (segment_get (&(dseg->seg), (char *)value, row, col) < 0)
	{
		G_warning ("dseg_get(): could not read segment file");
		return -1;
	}
	return 0;
}
