#include <stdio.h>
#include <stdlib.h>

int create_map (char *name, char *polyfile)
{
    char buf[1024];
    sprintf (buf, "r.in.poly rows=512 i='%s' o='%s'", polyfile,name);
    fprintf (stdout,"Creating raster map %s\n", name);
    return system(buf);
}
