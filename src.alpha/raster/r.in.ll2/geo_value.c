#include "geo.h"
#include "rowio.h"
geo_value (lat, lon, geo, geo_data)
    double lat, lon;
    struct GEO *geo;
    ROWIO *geo_data;
{
    int row, col;
    char *buf;
    char *rowio_get();

    row_col (geo, lat, lon, &row, &col);

    if (row < 0 || row >= geo->nrows) return 0;
    if (col < 0 || col >= geo->ncols) return 0;

    buf = rowio_get (geo_data, row);
    if (buf != NULL)
	return value (buf + col*geo->bpc, geo->bpc, geo->sflag);
    else
	return 0;
}
