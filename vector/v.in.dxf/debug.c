#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static int debug_on = 0;

int debuginit(void)
{

    if (getenv("DEBUG") != NULL)
	debug_on = 1;
    else
	debug_on = 0;

    return 0;
}

int debugf(char *format, ...)
{
    va_list a;

    va_start(a, format);
    if (debug_on)
	vfprintf(stderr, format, a);

    va_end(a);

    return 0;
}
