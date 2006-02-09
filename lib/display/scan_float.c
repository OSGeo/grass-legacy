#include <grass/display.h>

int D_scan_float ( char *buf, float *f)
{
    char dummy[2];

    *dummy = 0;
    return sscanf (buf, "%f%1s", f, dummy) == 1 && *dummy == 0 ;
}
