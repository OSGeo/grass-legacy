#include "gis.h"
#include "site.h"
#include "sv.h"

int 
readsites (FILE *fdsite, int all, int label, int verbose, Z **xyz)

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i, n, c, d, alloc = 1024;
  double ndesc, atof ();
  char desc[80];
  struct Cell_head window;
  Site *s;

  G_get_window (&window);

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");

  s = G_site_new_struct (c, 2, 0, 1);

  if (fdsite != stdin) {
    if (G_site_describe (fdsite, &n, &c, &i, &d) != 0)
      G_fatal_error ("failed to guess format");
    else
      d = 1;
  }

  if (d == 0)
  {
    fprintf (stderr, "\n");
    G_warning ("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
  }

  /* allocate chunk of memory */
  (*xyz) = (Z *) G_malloc (alloc * sizeof (Z));
  if ((*xyz) == NULL)
    G_fatal_error ("cannot allocate memory");
  i = 0;
  /* while (G_get_site (fdsite, &east, &north, &dum) > 0 ) */
  if (fdsite == stdin)
  {
    while (fscanf (fdsite, "%lf %lf %lf", &(s->east), &(s->north),
		   &(s->dbl_att[0])) == 3)
    {
      if (all || (s->east >= window.west && s->east <= window.east &&
		  s->north <= window.north && s->north >= window.south))
      {
	(*xyz)[i].z = s->dbl_att[0];
	(*xyz)[i].x = s->east;
	(*xyz)[i++].y = s->north;
      }
      if (i == alloc - 1)
      {
	alloc += 1024;
	(*xyz) = (Z *) G_realloc (*xyz, alloc * sizeof (Z));
	if ((*xyz) == NULL)
	  G_fatal_error ("cannot add memory");
      }
    }
  }
  else
  {
    while (G_site_get (fdsite, s) == 0)
    {
      if (all || (s->east >= window.west && s->east <= window.east &&
		  s->north <= window.north && s->north >= window.south))
      {
	/* sscanf (dum, "%lf ", &(*xyz)[i].z); */
	(*xyz)[i].z = s->dbl_att[0];
	(*xyz)[i].x = s->east;
	(*xyz)[i++].y = s->north;
      }
      if (i == alloc - 1)
      {
	alloc += 1024;
	(*xyz) = (Z *) G_realloc (*xyz, alloc * sizeof (Z));
	if ((*xyz) == NULL)
	  G_fatal_error ("cannot add memory");
      }
    }
  }
  fclose (fdsite);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}
