#include "gis.h"
#include <stdio.h>
/******************************************
 * $GISABSE/etc/echo [-n] args
 *
 * echos its args to stdout
 * suppressing the newline if -n specified
 *
 * replaces the standard UNIX echo which
 * varies from machine to machine
 *******************************************/
int 
main (int argc, char *argv[])
{
    int i;
    int newline;
    int any;

    newline = 1;
    any = 0;

    for (i = 1; i < argc; i++)
	if (strcmp (argv[i],"-n") == 0)
	    newline = 0;
	else
	    fprintf (stderr, "%s%s", any++?" ":"", argv[i]);
    if (any && newline)
	fprintf (stderr, "\n");

    exit(0);
}
