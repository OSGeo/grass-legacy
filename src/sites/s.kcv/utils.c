/*
 * Copyright (C) 1993-1994. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include "gis.h"
#include "kcv.h"

double 
myrand (void)
{
  int rand ();

  return (double) rand ();
}

FILE *
opensites (char *basename, int i, char *ext)

{
  FILE *fd;
  char filename[1024], errmsg[256];

  sprintf (filename, "%s-%s.%d", basename, ext, i);
  fd = G_fopen_sites_new (filename);
  if (fd == NULL)
  {
    sprintf (errmsg, "%s can't create sites file [%s]",
	     G_program_name (), filename);
    G_fatal_error (errmsg);
  }
  fprintf (fd, "name|%s\n", filename);
  fprintf (fd, "desc|%s partition\n", ext);
  return fd;
}

int 
dcmp (void *a, void *b)
{
  int result = 0;		/* integer to be returned */
  double diff;

  if ((diff = ((D *) a)->dist - ((D *) b)->dist) < 0.0)
    result = -1;
  else if (diff > 0.0)
    result = 1;
  return result;
}
