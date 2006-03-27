#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static int debug_on = 0;

void debug_init(void)
{
    debug_on = (getenv("DEBUG") != NULL);

    return;
}

void debug_msg(char *format, ...)
{
    va_list a;

    va_start(a, format);
    if (debug_on)
	vfprintf(stderr, format, a);
    va_end(a);

    return;
}
