/* %W% %G% */
#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf,
      "usage: %s [-v] accum=file thin=file iters=integer dthres=integer", me);
    G_fatal_error (buf);
    exit(1);
}
