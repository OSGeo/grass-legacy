#include <grass/gis.h>
#include "local_proto.h"

int die (char *a, char *b)
{
    char msg[256];

    sprintf (msg, "%s: %s %s", G_program_name(), a, b);
    G_fatal_error (msg);
}
