#include "gis.h"
#include "site.h"
#include "univar.h"

int readsites (FILE *fdsite, int all, int verbose, int field, Z **xyz)

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i, strs, dims,map_type,dbls,allocated=1000;
  /*       i     n     c       d */
  double east, north, ndesc, atof ();
  char desc[80];
  Site *s;
  extern struct Cell_head window;

  G_sleep_on_error (0);

  field -= 1;  /* field number -> array index */

  if (verbose)
    fprintf (stderr, "Reading sites list ...                  ");


  if (G_site_describe (fdsite, &dims, &map_type, &strs, &dbls)!=0)
    G_fatal_error("failed to guess format");
  s = G_site_new_struct (map_type, dims, strs, dbls);

  if(field >= dbls){
      G_fatal_error("decimal field not present in sites file");
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
  /* while (G_get_site (fdsite, &east, &north, &dum) > 0) */
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
#ifdef OLD_API
      if (label)
      {
	sscanf (dum, "#%*d %s", desc);
	ndesc = atof (G_strip (desc));
	if (ndesc == 0.0)
	{
	  G_warning ("possible non-numeric description field; setting to zero");
	  ndesc = 0;
	}
        (*xyz)[i].z = ndesc;
      }
      else
      {
        while (dum[0] == '#' || dum[0] == ' ' ) dum++;
	/* sscanf (dum, "%[^#]", desc);
	G_strip (desc);
fprintf(stderr,"DIAG: dum = |%s| desc=|%s|\n",dum,desc); */
	sscanf (dum, "%lf ", &(*xyz)[i].z);
      }
#endif /* OLD_API */
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
