#include "gis.h"
#include "cseg.h"

dseg_put (dseg, value, row, col)
	DSEG	*dseg;
	int	row, col;
	double	*value;
{
	if (segment_put (&(dseg->seg), (char *) value, row, col) < 0)
	{
		G_warning ("dseg_put(): could not write segment file");
		return -1;
	}
	return 0;
}
