#include <grass/display.h>

int D_scan_int ( char *buf, int *f)
{
    char dummy[2];

    *dummy = 0;
    return sscanf (buf, "%d%1s", f, dummy) == 1 && *dummy == 0 ;
}
