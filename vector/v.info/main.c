/***************************************************************
 *
 * MODULE:       v.info
 * 
 * AUTHOR(S):    CERL, updated to 5.7 by Markus Neteler
 *               
 * PURPOSE:      print vector map info
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
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
  struct Flag *histf;
  struct Map_info Map;
  struct dig_head v_head;
  BOUND_BOX box;
  char *mapset, line[200], buf[1001];
  int i;
  int with_z;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description = "Outputs basic information about a user-specified vector map layer.";

  /* get G_OPT_ from include/gis.h */
  in_opt = G_define_standard_option(G_OPT_V_MAP);

  histf = G_define_flag ();
  histf->key             = 'h';
  histf->description     = "Print vectory history instead of info";

  if (G_parser(argc,argv))
    exit(1);

  /* open input vector */
  if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
     G_fatal_error ("Could not find input map <%s>\n", in_opt->answer);
  }
    
  Vect_set_open_level (2);
  Vect_open_old_head (&Map, in_opt->answer, mapset);
  with_z = Vect_is_3d (&Map);
  v_head = Map.head;

  if ( histf->answer ) {
      Vect_hist_rewind ( &Map );
      while ( Vect_hist_read ( buf, 1000, &Map ) != NULL ) {
	 fprintf ( stdout, buf );
      } 
  } else { 
      divider ('+');
      sprintf (line, "Layer:    %-29.29s  Organization: %s", in_opt->answer, Vect_get_organization(&Map));
      printline (line);
      sprintf (line, "Mapset:   %-29.29s  Source Date: %s", mapset, Vect_get_map_date(&Map));
      printline (line);
      sprintf (line, "Location: %-29.29s  Name of creator: %s", G_location(), Vect_get_person(&Map));
      printline (line);
      sprintf (line, "Database: %s", G_gisdbase() );
      printline (line);
      sprintf (line, "Title:    %s", Vect_get_map_name(&Map) );
      printline (line);
      sprintf (line, "Map Scale:  1:%d", Vect_get_scale(&Map));
      printline (line);
      sprintf (line, "Map format: %s",  Vect_maptype_info(&Map) );
      printline (line);

      divider ('|');

      sprintf (line, "  Type of Map:  %s (level: %i)        ", "Vector", Vect_level (&Map));

      printline (line);

      if ( Vect_level (&Map) > 1)
      {
	printline ("");
	sprintf (line, "  Number of points:       %-9ld       Number of areas:      %-9ld", 
			  (long)Vect_get_num_primitives(&Map, GV_POINT), (long)Vect_get_num_areas(&Map));
	printline (line);
	sprintf (line, "  Number of lines:        %-9ld       Number of islands:    %-9ld",
			  (long)Vect_get_num_primitives(&Map, GV_LINE), (long)Vect_get_num_islands(&Map));
	printline (line);
	sprintf (line, "  Number of boundaries:   %-9ld       Number of faces:      %-9ld",
			  (long)Vect_get_num_primitives(&Map, GV_BOUNDARY), (long)Vect_get_num_primitives(&Map, GV_FACE));
	printline (line);
	sprintf (line, "  Number of centroids:    %-9ld       Number of kernels:    %-9ld",
			  (long)Vect_get_num_primitives(&Map, GV_CENTROID), (long)Vect_get_num_primitives(&Map, GV_KERNEL));
	printline (line);
	printline ("");
	sprintf (line, "  Map is 3D:              %d", Vect_is_3d(&Map));
	printline (line);
	sprintf (line, "  Number of dblinks:      %-9ld", (long)Vect_get_num_dblinks(&Map));
	printline (line);
      }
      else
      { /* should not be reached */
	sprintf (line, "                No topology present.");
	printline (line);
      }

      printline ("");
      sprintf (line, "  Projection: %s (zone %d)", G_database_projection_name(), G_zone());
      printline (line);

      Vect_get_map_box (&Map, &box );
      sprintf (line, "           N: %-10.3f    S: %-10.3f", box.N, box.S);
      printline (line);
      sprintf (line, "           E: %-10.3f    W: %-10.3f", box.E, box.W);
      printline (line);
      sprintf (line, "           B: %-6.3f         T: %-6.3f", box.B, box.T);
      printline (line);

      printline ("");
      sprintf (line, "  Digitize threshold: %.5f", Vect_get_thresh(&Map));
      printline (line);
      sprintf (line, "  Comments:");
      printline (line);
      sprintf (line, "    %s", v_head.line_3);
      printline (line);
      divider ('+');
      fprintf (stdout, "\n");
  }
 
  Vect_close (&Map);

  return (0);
}

