/*
 * Copyright (C) 1994-1995. James Darrell McCauley. (darrell@mccauley-usa.com)
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include "gis.h"
#include "site.h"

int readz (
  FILE *fdsite,
  int verbose,
  int field,
  double **z,
  struct Cell_head window)
{
  char *dum;
  int i, alloced = 1000,dims,map_type,strs,dbls;
  Site *mysite;               /* pointer to Site */

  G_sleep_on_error (0);

  field -= 1;  /* field number -> array index */
  
  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  if (G_site_describe (fdsite, &dims, &map_type, &strs, &dbls)!=0)
    G_fatal_error("failed to guess format");
  
  mysite = G_site_new_struct (map_type, dims, strs, dbls);
G_warning("ss:%i",field);
  if(field >= dbls){
      G_fatal_error("\n decimal field %i not present in sites file", field + 1);
  }

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
    if (G_site_in_region(mysite, &window))
    {
      if (i == alloced - 1)
      {
	alloced += 1000;
	*z = (double *) G_realloc (*z, alloced * sizeof (double));
	if (*z == NULL)
	  G_fatal_error ("cannot allocate memory");
      }
      (*z)[i++]=mysite->dbl_att[field];
    }
  }
  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
