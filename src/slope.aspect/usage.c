/* %W% %G% */
#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf,
      "usage: %s [-v] elevation=input slope=output1 aspect=output2", me);
    G_fatal_error (buf);
    exit(1);
}
