#include "gis.h"
scan_declination (s, decl)
    char *s;
    double *decl;
{
    int sign;

    sign = 1;
    if (*s == '-')
    {
	sign = -1;
	s++;
    }
    if (!G_scan_resolution(s, decl, PROJECTION_LL))
	return 0;
    
    *decl *= sign;
    return 1;
}
