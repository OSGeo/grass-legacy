#include "gis.h"
die (a, b) char *a, *b;
{
    char msg[256];

    sprintf (msg, "%s: %s %s", G_program_name(), a, b);
    G_fatal_error (msg);
}
