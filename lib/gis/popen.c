#include <stdio.h>
#include <grass/gis.h>

FILE *G_popen(const char *cmd, const char *mode)
{
    return popen(cmd, mode);
}

int G_pclose(FILE * ptr)
{
    return pclose(ptr);
}
