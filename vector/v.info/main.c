/***************************************************************
 *
 * MODULE:       v.clean
 * 
 * AUTHOR(S):    CERL, updated to 5.1 by Markus Neteler
 *               
 * PURPOSE:      Clean lines
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 * TODO:  improve this, check for level 1 data, check for categories
 *
 **************************************************************/
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

#define printline(x) fprintf (stdout, " | %-74.74s |\n", x)
#define divider(x) \
	fprintf (stdout, " %c", x); \
	for (i = 0; i < 76; i++ ) \
		fprintf ( stdout, "-" ); \
	fprintf (stdout, "%c\n", x)

/* the vector header is here:
   include/vect/dig_structs.h
   
   the vector API is here:
   lib/vector/Vlib/level_two.c
*/

int
main (int argc, char *argv[])
{
  struct GModule *module;
  struct Option *in_opt;
  struct Map_info Map;
  struct Categories cats;
  struct dig_head v_head;
  BOUND_BOX box;
  char *mapset, line[200], temp[50];
  int cats_ok, i;
  int with_z;

  module = G_define_module();
  module->description =
	"Outputs basic information about a user-specified vector map layer.";

  /* get G_OPT_ from include/gis.h */
  in_opt = G_define_standard_option(G_OPT_V_MAP);

  /* is the G_gisinit() position correct here? see also v.clean */
  G_gisinit (argv[0]);
  if (G_parser(argc,argv))
    exit(1);

  /* open input vector */
  if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
     G_fatal_error ("Could not find input %s\n", in_opt->answer);
  }
    
  Vect_set_open_level (2);
  Vect_open_old (&Map, in_opt->answer, mapset);
  with_z = Vect_is_3d (&Map);

  Vect_set_fatal_error (GV_FATAL_PRINT);

  if ((cats_ok=G_read_vector_cats (in_opt->answer, mapset, &cats)) < 0) {
      G_warning ("Could not find category file for %s", in_opt->answer);
  }
  v_head = Map.head;

  divider ('+');

  sprintf (line, "Mapset:   %-29.29s  Organization: %s", mapset, Vect_get_organization(&Map));
  printline (line);
  sprintf (line, "Layer:    %-29.29s  Source Date: %s", in_opt->answer, Vect_get_map_date(&Map));
  printline (line);
  sprintf (line, "Orig. Scale: 1:%d", Vect_get_scale(&Map));
  printline (line);
  sprintf (line, "Location: %-29.29s  Name of creator: %s", G_location(), Vect_get_person(&Map));
  printline (line);
  sprintf (line, "DataBase: %s", G_gisdbase ());
  printline (line);
  sprintf (line, "Title:    %s", Vect_get_map_name(&Map));
  printline (line);

  divider ('|');
  printline ("");

  sprintf (line, "  Type of Map:  %s (level: %i)        ", "Vector", Vect_level (&Map));

  strcat (line, "Number of categories: ");

  if (cats_ok > 0)
  {
    sprintf (temp, "%-9ld", (long)cats.num);
    strcat (line, temp);
  }
  else
    strcat (line, "??");
  printline (line);

  if ( Vect_level (&Map) > 1)
  {
    sprintf (line, "                                         Number of points:     %-9ld", (long)Vect_get_num_nodes(&Map));
    printline (line);
    sprintf (line, "                                         Number of lines:      %-9ld", (long)Vect_get_num_lines(&Map));
    printline (line);
    sprintf (line, "                                         Number of centroids:  %-9ld", (long)Vect_get_num_centroids(&Map));
    printline (line);
    sprintf (line, "                                         Number of areas:      %-9ld", (long)Vect_get_num_areas(&Map));
    printline (line);
    sprintf (line, "                                         Number of islands:    %-9ld", (long)Vect_get_num_islands(&Map));
    printline (line);
    sprintf (line, "                                         Map is 3D:            %d", Vect_is_3d(&Map));
    printline (line);
  }
  else
  {
    sprintf (line, "                No topology present.");
    printline (line);
  }

  printline ("");
  sprintf (line, "  Projection: %s (zone %d)", G_database_projection_name(), G_zone());
  printline (line);

  Vect_get_map_box (&Map, &box );
  sprintf (line, "           N: %10.0f    S: %10.0f", box.N, box.S);
  printline (line);
  sprintf (line, "           E: %10.0f    W: %10.0f", box.E, box.W);
  printline (line);

  printline ("");
  sprintf (line, "  Digitize threshold: %.5f", Vect_get_thresh(&Map));
  printline (line);
  sprintf (line, "  Comments:");
  printline (line);
  sprintf (line, "    %s", v_head.line_3);
  printline (line);
  printline ("");
  divider ('+');
  fprintf (stdout, "\n");
 
  Vect_close (&Map);

  return (0);
}

