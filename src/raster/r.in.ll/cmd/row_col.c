#include "geo.h"
row_col (geo, lat, lon, row, col)
    struct GEO *geo;
    double lat, lon;
    int *row, *col;
{
    extern double floor();
    *row = floor((geo->lat - lat) / geo->lat_res);
    *col = floor((geo->lon - lon) / geo->lon_res);
}
