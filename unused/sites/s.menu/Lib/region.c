#include "gis.h"

char *format_res (double x, char *buf, int projection)
{
	G_format_resolution (x, buf, projection);
	return buf;
}

char *format_east (double x, char *buf, int projection)
{
	G_format_easting (x, buf, projection);
	return buf;
}

char *format_north (double x, char *buf, int projection)
{
	G_format_northing (x, buf, projection);
	return buf;
}

int scan_north (char *buf, double *x)
{
	return G_scan_northing (buf, x, G_projection());
}

int scan_east (char *buf, double *x)
{
	return G_scan_easting (buf, x, G_projection());
}

int scan_res (char *buf, double *x)
{
        return G_scan_resolution (buf, x, G_projection());
}
