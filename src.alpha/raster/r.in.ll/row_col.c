#include "geo.h"
#include <math.h>
row_col (geo, lat, lon, row, col)
    struct GEO *geo;
    double lat, lon;
    int *row, *col;
{
    *row = floor (((geo->lat - lat) / geo->lat_res) + 0.5);
    *col = floor (((geo->lon - lon) / geo->lon_res) + 0.5);
}
