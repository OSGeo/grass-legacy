#include <stdio.h>
char *
drivers (char *name)
{
    static char path[1024];
    char *G_gisbase();

    sprintf (path, "%s/etc/paint/driver.sh/%s", G_gisbase(), name);
    return path;
}
