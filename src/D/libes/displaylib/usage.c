/*  %W%  %G%  */

#include <stdio.h>

D_usage (me, rule)
    char *me;
    char *rule;
{
    fprintf (stderr, "D_usage: %s %s\n", me, rule);
    return -1;
}
