#include <stdio.h>
#include "gis.h"

int input (char *buf)
{
    if (!fgets(buf,5,stdin)) exit(0);
    G_strip (buf);
    if (strncmp (buf,"exit",4)==0) exit(0);
}
