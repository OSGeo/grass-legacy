#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <assert.h>
#include <grass/gis.h>

/* IMPORTANT NOTE:
   the use of snprintf()/G_snprintf() is discouraged in favour
   of calculating how long the string will be and allocating
   enough memory!
 */

/* TODO: if needed, implement alternative versions for portability.
   potential code source:
    - http://www.ijs.si/software/snprintf/
    - openssh's snprintf() implementation: bsd-snprintf.c
 */

/* #ifdef HAVE_SNPRINTF */

int G_snprintf(char *str, size_t size, const char *fmt, ...)
{
    va_list ap;
    int count;

    va_start(ap, fmt);
    count = vsnprintf (str, size, fmt, ap);
    va_end(ap);

    return count;
}

/* #endif */
