#include "gis.h"

static int format_double(double,char *);

int G_format_northing ( double north, char *buf,int projection)
{
    if (projection == PROJECTION_LL)
	G_lat_format (north, buf);
    else
	format_double (north, buf);

    return 0;
}

int G_format_easting ( double east, char *buf,int projection)
{
    if (projection == PROJECTION_LL)
	G_lon_format (east, buf);
    else
	format_double (east, buf);

    return 0;
}

int G_format_resolution ( double res, char *buf,int projection)
{
    if (projection == PROJECTION_LL)
	G_llres_format (res, buf);
    else
	format_double (res, buf);

    return 0;
}

static int format_double ( double value, char *buf)
{
    sprintf (buf, "%.8f", value);
    G_trim_decimal (buf);

    return 0;
}
