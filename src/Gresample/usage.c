#include <stdio.h>
usage(me) char *me;
{
    fprintf (stderr, "usage: %s [-v] input_layer output_layer\n", me);
    sleep(3);
    exit(1);
}
