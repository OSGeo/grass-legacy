/*
 * Copyright (C) 1995.  Bill Brown <brown@gis.uiuc.edu> & Michael Shapiro
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */
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
