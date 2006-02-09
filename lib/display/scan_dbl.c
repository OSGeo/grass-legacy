#include <grass/display.h>

int D_scan_double (char *buf, double *f)
{
    char dummy[2];

    *dummy = 0;
    return sscanf (buf, "%lf%1s", f, dummy) == 1 && *dummy == 0 ;
}
