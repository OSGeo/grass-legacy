#include "geo.h"
#include "rowio.h"
#include "local_proto.h"

int geo_value (double lat, double lon, struct GEO *geo, ROWIO *geo_data)
{
    int row, col;
    char *buf;

    row_col (geo, lat, lon, &row, &col);

    if (row < 0 || row >= geo->nrows) return 0;
    if (col < 0 || col >= geo->ncols) return 0;

    buf = rowio_get (geo_data, row);
    if (buf != NULL)
	return value ((unsigned char *) buf + col*geo->bpc, geo->bpc, geo->sflag);
    else
	return 0;
}
