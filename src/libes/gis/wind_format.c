#include "gis.h"

G_format_northing (north, buf, projection)
    double north;
    char *buf;
{
    if (projection == PROJECTION_LL)
	G_lat_format (north, buf);
    else
	format_double (north, buf);
}

G_format_easting (east, buf, projection)
    double east;
    char *buf;
{
    if (projection == PROJECTION_LL)
	G_lon_format (east, buf);
    else
	format_double (east, buf);
}

G_format_resolution (res, buf, projection)
    double res;
    char *buf;
{
    if (projection == PROJECTION_LL)
	G_llres_format (res, buf);
    else
	format_double (res, buf);
}

static
format_double (value, buf)
    double value;
    char *buf;
{
    sprintf (buf, "%.8lf", value);
    G_trim_decimal (buf);
}
