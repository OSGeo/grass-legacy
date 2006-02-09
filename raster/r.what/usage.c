#include <stdlib.h>
#include <grass/gis.h>
#include "local_proto.h"

int usage (void)
{
    fprintf (stderr, "%s [-fci] [null=string] layer1 [layer2] ...\n", G_program_name());
    exit (1);
}
