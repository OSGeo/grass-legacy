#include "gis.h"
#include "proto.h"

char *drivers( char *name)
{
    static char path[1024];

    sprintf (path, "%s/etc/paint/driver.sh/%s", G_gisbase(), name);
    return path;
}
