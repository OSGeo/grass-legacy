/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 *
 * $Id$
 */

#include "gis.h"
#include "site.h"

int readz (fdsite, verbose, z, window, all)
  FILE *fdsite;
  int verbose, all;
  double **z;
  struct Cell_head window;
{
  char *dum;
  int i, alloced = 1000,dims,cat,strs,dbls;
  Site *mysite;               /* pointer to Site */

  G_sleep_on_error (0);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  mysite = G_site_new_struct (cat,dims,strs,dbls);

  if (G_site_describe (fdsite, &dims, &cat, &strs, &dbls)!=0)
    G_fatal_error("failed to guess format");

  if (dbls==0)
  {
    fprintf(stderr,"\n");
    G_warning("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
  }
  /* allocate chunk of memory */
  if ((*z = G_alloc_vector (alloced)) == NULL)
    G_fatal_error ("cannot allocate memory");
  i = 0;
  while ((G_site_get (fdsite, mysite)) == 0) 
  {
    if (G_site_in_region(mysite, &window) || all)
    {
      if (i == alloced - 1)
      {
	alloced += 1000;
	*z = (double *) G_realloc (*z, alloced * sizeof (double));
	if (*z == NULL)
	  G_fatal_error ("cannot allocate memory");
      }
      (*z)[i++]=mysite->dbl_att[0];
    }
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
