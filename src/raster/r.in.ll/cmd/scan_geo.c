#include "gis.h"
scan_lat(buf, result)
    char *buf;
    double *result;
{
    if (!G_scan_northing (buf, result, PROJECTION_LL))
	return 0;
    *result *= 3600 ; /* conver to arc seconds for CC library */;
    return 1;
}
scan_lon(buf, result)
    char *buf;
    double *result;
{
    if (!G_scan_easting (buf, result, PROJECTION_LL))
	return 0;
    *result *= 3600 ; /* convert to arc seconds for CC library */;
    *result *= -1;    /* invert sense of east and west */
    return 1;
}
