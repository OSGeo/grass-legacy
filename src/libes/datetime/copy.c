#include "datetime.h"

static void 
copy (char *dst, char *src, int n)
{
      while (n-- > 0)
  	*dst++ = *src++;
}
  
void 
datetime_copy (DateTime *dst, DateTime *src)
{
    copy ((char *)dst, (char *)src, sizeof(DateTime));
}
