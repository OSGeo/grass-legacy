/*
 * int G_readsites (FILE *fdsite, int all, int verbose, int dec_field, site **xyz)
 *   reads in a sites list into site struct
 *   returning the number of sites found
 *   select field (table column) by dec_field parameter
 *
 * Markus Neteler
 * neteler@geog.uni-hannover.de
 * 7/2000
 * based on support.c by Job Spijker (v.bubble)
 *
 * TODO:
 *   - only 2D sites supported
 *   - no string support yet
 *   - problems, if double entries missing in second row
 */

#include "readsites.h"

int G_readsites (FILE *fdsite, int all, int verbose, int dec_field, site **xyz)

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i, strs, dims, map_type, dbls, allocated=1000;
  double east, north, ndesc, atof ();
  char desc[80];
  Site *s;
  
  /*a window can be used to define a area, it's not implemented yet */
  struct Cell_head window;

  G_sleep_on_error (0);

  dec_field -= 1;  /* decimal field number -> array index */

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  /* note: G_site_describe only reads first record to guess format */
  if (G_site_describe (fdsite, &dims, &map_type, &strs, &dbls)!=0)
    G_fatal_error("failed to guess format");
  s = G_site_new_struct (map_type, dims, strs, dbls);

  if(dec_field >= dbls){
      fprintf(stderr,"\n");
      G_fatal_error("selected decimal field column no. %d not present in sites list.", dec_field+1);
  }

  if (dbls==0)
  {
    fprintf(stderr,"\n");
    G_warning("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
  }

  /* add multi-dimension support here based on dims !*/
  
  
  /* allocate chunk of memory */
  (*xyz) = (site *) G_malloc (allocated * sizeof (site ));
  if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");

  i = 0;

  while (G_site_get (fdsite, s) == 0) 
  {
    if (i == allocated)
    {
      allocated+=1000;
      (*xyz) = (site *) G_realloc ((*xyz), allocated * sizeof (site));
      if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");
    }
    if (all || (s->east >= window.west && s->east <= window.east &&
		s->north <= window.north && s->north >= window.south))
    {
      (*xyz)[i].z=s->dbl_att[dec_field];
      (*xyz)[i].x=s->east;
      (*xyz)[i++].y=s->north;
    }
  }
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
