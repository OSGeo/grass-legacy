#include "datetime.h"

static void 
copy (char *src, char *dst, int n)
{
      while (n-- > 0)
  	*dst++ = *src++;
}
  
void 
datetime_copy (DateTime *src, DateTime *dst)
{
      copy ((char *)src, (char *)dst, sizeof(DateTime));
}
