#include "gis.h"
#include "fullwindow.h"

scan_easting (buf, f)
    char *buf;
    double *f;
{
    if(scan_percent (buf, f, fullwindow.west, fullwindow.east))
	return 1;
    return G_scan_easting (buf, f, fullwindow.proj);
}

scan_northing (buf, f)
    char *buf;
    double *f;
{
    if(scan_percent (buf, f, fullwindow.south, fullwindow.north))
	return 1;
    return G_scan_northing (buf, f, fullwindow.proj);
}

scan_resolution (buf, f)
    char *buf;
    double *f;
{
    return G_scan_resolution (buf, f, fullwindow.proj);
}

static
scan_percent (buf, f, min, max)
    char *buf;
    double *f;
    double min, max;
{
    char percent[3];
    *percent = 0;
    if (sscanf (buf, "%lf%2s", f, percent) != 2)
	return 0;
    if (strcmp (percent, "%") != 0)
	return 0;
    *f = min + (max-min) * (*f/100.0);
    return 1;
}
