#include <stdio.h>
#include "gis.h"

char *mygets(char *buf)
{
    if(!fgets(buf,256,stdin))
    {
        fprintf (stdout,"EOF\n");
        exit(0);
    }
    G_strip(buf);
    return buf;
}
