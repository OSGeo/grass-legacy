/* @(#)usage.c	2.1   6/26/87 */

#include <stdio.h>

D_usage (name, command)
    char *name, *command;
{
    fprintf (stderr, "D_usage: %s %s\n", name, command);
    exit(1);
}
