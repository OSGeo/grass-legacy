#include <stdio.h>
#include <stdarg.h>

int 
debugf (char *t,...)
{
  va_list ap;

  va_start (ap, t);
  vfprintf (stderr, t, ap);
  va_end (ap);

  return 0;
}
