#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define DEBUG

static int Debug_on = 0;

int debuginit (void)
{
    
    if ((getenv ("DEBUG")) != NULL)
	Debug_on = 1;
    else
	Debug_on = 0;

    return 0;
}

int debugf (char *format, ...)
{
    va_list a;

    va_start(a,format);
    if (Debug_on)
	vfprintf (stderr, format, a);

    va_end(a);

    return 0;
}
