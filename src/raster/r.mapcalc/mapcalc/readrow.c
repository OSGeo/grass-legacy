#include "gis.h"

static int fp = 0;

void 
set_readrow_for_fp (int flag)
{
    fp = flag;
}

int readrow(int fd, char *buf,int row,int dummy)
{
    if (fp)
	return G_get_d_raster_row (fd, (DCELL *)buf, row) >= 0;
    else
	return G_get_c_raster_row (fd, (CELL *)buf, row) >= 0;
}
