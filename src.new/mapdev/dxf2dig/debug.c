#include <stdio.h>

#define DEBUG

static int Debug_on = 0;

debuginit ()
{
    
    if ((getenv ("DEBUG")) != NULL)
	Debug_on = 1;
    else
	Debug_on = 0;
}

debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    if (Debug_on)
	fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
