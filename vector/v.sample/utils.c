/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include <string.h>
#include "gis.h"


double scancatlabel(str)
 char *str;
{
  double val;

  if (strcmp(str,"no data") != 0)
    sscanf(str, "%lf", &val);
  else
  {
    G_warning("\"no data\" label found; setting to zero");
    val=0.0;
  }

  return val;
}

