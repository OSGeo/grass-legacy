#include <string.h>
#include "gis.h"
#include "Vect.h"

struct site_list
{
  double east;
  double north;
  long att;
  char *att_str;
};

void 
write_cats_att (char *outname, FILE *fd_att, FILE *fd_site, int all, int verbose)
{
  char *desc, *temp;
  int n,i = 0, all_desc_ok = 1, some_desc_ok = 0;
  long att_num;
  double east, north;
  struct site_list *list;
  struct Categories *pcats;
  extern struct Cell_head window;

  list = NULL;
  if (verbose)
    fprintf (stderr, "Writing Cats/Atts files ...         ");

  for (n = 0; G_get_site (fd_site, &east, &north, &desc) > 0; ++n)
  {
    if (all || (east >= window.west && east <= window.east &&
		north <= window.north && north >= window.south))
    {
      if (n % 50 == 0)
	list = (struct site_list *) G_realloc (list, (n + 50) * sizeof (struct site_list));
      list[n].east = east;
      list[n].north = north;
      if (*desc == '#')
	desc++;
      temp = G_malloc (strlen (desc) + 1);
      *temp = 0;
      att_num = 0;
      if (sscanf (desc, "%ld%[^\n]", &att_num, temp) < 1)
	all_desc_ok = 0;
      else
	some_desc_ok++;
      list[n].att = att_num;
      list[n].att_str = temp;
    }
    else 
      n--;
  }

  if (!all_desc_ok && some_desc_ok)
  {
    fprintf (stderr, "Some site descr. not in proper format.\n");
    fprintf (stderr, "Must be '#number description'.\n");
    fprintf (stderr, "Setting all attributes to '1' in vector file.\n");
    all_desc_ok=0;
  }

  G_init_cats (n+1, "Output of s.voronoi", pcats);

  for (i = 0; i < n; ++i)
  {
    if (!all_desc_ok)
    {
      fprintf (fd_att, "A  %9.2f %9.2f %8d\n",
	       list[i].east, list[i].north, 1);
      G_set_cat (i, NULL, pcats);
    }
    else
    {
      fprintf (fd_att, "A  %9.2f %9.2f %8ld\n",
	       list[i].east, list[i].north, list[i].att);
      G_set_cat (i+1, list[i].att_str, pcats);
    }
    if (verbose)
      G_percent (i, n, 10);
  }

  if (G_write_vector_cats (outname, pcats) < 0)
    G_warning ("Error writing dig_cats file");

  G_free_cats (pcats);
  G_free(list);
  if (verbose)
    G_percent (1, 1, 2);
}
