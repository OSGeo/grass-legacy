#include "geo.h"

int row_col (struct GEO *geo, double lat, double lon, int *row, int *col)
{
    extern double floor();
    *row = floor((geo->lat - lat) / geo->lat_res);
    *col = floor((geo->lon - lon) / geo->lon_res);

    return 0;
}
