/* %W% %G% */
#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf, "usage: %s [-v] elev=file accum=file drain=file east=UTM north=UTM sthres=float pthres=int [lake=file]", me);
    G_fatal_error (buf);
    exit(1);
}
