/* %W% %G% */
#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf, "usage: %s [-v] in=file out=file passes=integer", me);
    G_fatal_error (buf);
    exit(1);
}
