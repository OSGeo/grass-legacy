/*
 * int G_readsites 
 *     (FILE *fdsite, int all, int verbose, int dec_field, Z **xyz)
 *   reads in a sites list into site struct
 *   returning the number of sites found
 *   select decimal field (table column) by dec_field parameter
 *   closes fdsite on exit
 *
 * Markus Neteler
 * neteler@geog.uni-hannover.de
 *
 * $Id$
 *
 * based on support.c by Job Spijker (v.bubble)
 *
 * TODO:
 *   - no string support yet
 *   - problems, if double entries missing in second row
 */


#include "gis.h"
#include "readsites.h"

int G_readsites (FILE *fdsite, int all, int verbose, int field, Z **xyz)

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i, strs, dims,map_type,dbls,allocated=1000;
  double east, north, ndesc, atof ();
  char desc[80];
  Site *s;
  extern struct Cell_head window;

  G_sleep_on_error (0);

  field -= 1;  /* field number -> array index */

  if (verbose)
    fprintf (stderr, "Reading sites list ...                  ");

  /* check structure from first row in sites list */
  if (G_site_describe (fdsite, &dims, &map_type, &strs, &dbls)!=0)
    G_fatal_error("failed to guess format");
  s = G_site_new_struct (map_type, dims, strs, dbls);

  if(field >= dbls){
      G_fatal_error("decimal field %i not present in sites file", field + 1);
  }

  if (dbls==0)
  {
    fprintf(stderr,"\n");
    G_warning("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
  }

  /* allocate chunk of memory */
  (*xyz) = (Z *) G_malloc (allocated * sizeof (Z));
  if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");

  i = 0;
  while (G_site_get (fdsite, s) == 0) 
  {
    if (i == allocated)
    {
      allocated+=1000;
      (*xyz) = (Z *) G_realloc ((*xyz), allocated * sizeof (Z));
      if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");
    }
    if (all || (s->east >= window.west && s->east <= window.east &&
		s->north <= window.north && s->north >= window.south))
    {
      (*xyz)[i].z=s->dbl_att[field];
      (*xyz)[i].x=s->east;
      (*xyz)[i++].y=s->north;
    }
  }

  fclose (fdsite);
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
