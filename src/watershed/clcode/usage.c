/* %W% %G% */
#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf,
      "usage: %s [-v] accum=file stream=file drain=file [out=file]", me);
    G_fatal_error (buf);
    exit(1);
}
