#include "gis.h"
#include "cseg.h"

int 
seg_get (SSEG *sseg, char *value, int row, int col)
{
	if (segment_get (&(sseg->seg), value, row, col) < 0)
	{
		G_warning ("seg_get(): could not read segment file");
		return -1;
	}
	return 0;
}
