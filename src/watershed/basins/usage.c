/* %W% %G% */
#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf, "usage: %s [-v] drain=file stream=file basin=file", me);
    G_fatal_error (buf);
    exit(1);
}
