/*
 * Copyright (C) 1993-1994. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include "gis.h"
#include "kcv.h"

void 
d_reset (D **d, int n)
{
  int i;

  for (i = 0; i < n; ++i)
    (*d)[i].i = -1;
  return;
}
