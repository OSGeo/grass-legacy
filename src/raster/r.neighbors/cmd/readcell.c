#include "gis.h"
#include "ncb.h"

readcell (fd, row, nrows, ncols)
{
    rotate_bufs();

    if (row < nrows)
	G_get_map_row (fd, ncb.buf[ncb.nsize-1] + ncb.dist, row);
    else
	G_zero(ncb.buf[ncb.nsize-1] + ncb.dist, ncols * sizeof(CELL));
}
