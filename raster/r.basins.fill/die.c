#include <stdlib.h>
#include "gis.h"

int die (char *name, char *mapset, char *msg)
{
    char err[100];

    sprintf (err, "%s [%s] in [%s]", msg, name, mapset);
    G_fatal_error (err);
    exit(1);
}
