/****************************************************************************
 *
 * MODULE:       v.build.polylines
 * AUTHOR(S):    Mark Lake (original contributor)
 *               Major rewrite by Radim Blazek, October 2002
 *               Glynn Clements <glynn gclements.plus.com>, Markus Neteler <neteler itc.it>
 * PURPOSE:      
 * COPYRIGHT:    (C) 2002-2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/
/*
  v.build.polylines

  *****
 
  Mark Lake  5/4/00

  University College London
  Institute of Archaeology
  31-34 Gordon Square
  London.  WC1H 0PY

  Email: mark.lake@ucl.ac.uk

  Updated to grass 5.7 by Radim Blazek
  
  *****

  PURPOSE 

  1) Convert lines or mixed lines and polylines to polylines.  Preserve points 
  if present.  Allow the user to label the new lines as lines or area edges.

  *****

  METHOD

  1) A line is a single straight line segment defined by one start
  node, one end node and no other nodes.  In contrast, a polyline
  consists of a number of straight line segments each joined by a common
  node which is connected to exactly two lines.  The start and end nodes
  of the polyline are connected to either one line, or three or more
  lines.

  Points and centroids are ignored by build process and copied to output vector.

  *****

  FILES

  1) main.c - algorithm for ifs.

  2) walk.c - functions to find start of polylines and to pick up their
  coordinates.

  3) line_coords.c - structure for storing coordinates.

  *****

  PORTABILITY

  1) Portable

***********************************************************************/
#define MAIN

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>
#include "global.h"
#include "walk.h"

int main (int argc, char **argv)
{
  int line;
  struct line_pnts *points;
  struct line_cats *Cats;
  
  struct Map_info map, Out;
  struct GModule *module;
  struct Option *input;
  struct Option *output;
  struct Flag *quietly;

  int polyline;
  int *lines_visited;
  int points_in_polyline;
  int start_line;

  char *mapset;
  int start_type;

  /*  Initialize the GIS calls */
  G_gisinit(argv[0]) ;

  module = G_define_module();
  module->keywords = _("vector, topology");
  module->description = _("Build polylines from lines.");

  /* Define the options */

  input = G_define_standard_option (G_OPT_V_INPUT);
  output = G_define_standard_option (G_OPT_V_OUTPUT);

  quietly = G_define_flag ();
  quietly->key = 'q';
  quietly->description = _("Do not print polyline info");

  if (G_parser(argc, argv)) exit (EXIT_FAILURE);
  
  Vect_check_input_output_name ( input->answer, output->answer, GV_FATAL_EXIT );

  /* Open binary vector map at level 2 */
  mapset = G_find_vector2 (input->answer, "");
  if ( mapset == NULL ) G_fatal_error ( "Could not find input map <%s>\n", input->answer);  
  Vect_set_open_level (2);
  Vect_open_old (&map, input->answer, mapset);

  /* Open new vector */
  G_find_vector2 ( output->answer, "");
  Vect_open_new ( &Out, output->answer, Vect_is_3d(&map) );

  /* Copy header info. */
  Vect_copy_head_data (&map, &Out); 

  /* History */
  Vect_hist_copy (&map, &Out);
  Vect_hist_command ( &Out );      

  /* Get the number of lines in the binary map and set up record of lines visited */

  lines_visited = (int*) G_calloc ( Vect_get_num_lines(&map) + 1, sizeof (int));
  if (lines_visited == NULL) G_fatal_error ("Error allocating memory for `lines_visited' ");

  G_message (_("The number of lines in the binary map is %d"), Vect_get_num_lines(&map) );

  /* Set up points structure and coordinate arrays */
  points = Vect_new_line_struct ();
  Cats = Vect_new_cats_struct ();

  /* Step over all lines in binary map */
  polyline = 0;

  for ( line = 1; line <= Vect_get_num_lines(&map); line++ ) {
      /* Skip line if already visited from another */
      if (lines_visited [line]) continue;
     
      /* Only get here if line is not previously visited */
      
      /* Find start of this polyline */
      start_line = walk_back (&map, line);
      start_type = Vect_read_line ( &map, NULL, NULL, start_line );
      if (!quietly->answer) {
            fprintf (stdout, "Polyline %d: start line = %d \n", polyline, start_line);
            fflush (stdout);
      }

      /* Walk forward and pick up coordinates */
      points_in_polyline = walk_forward_and_pick_up_coords (&map, start_line, points, lines_visited);

      /* Write the line (type of the first line is used) */
      Vect_write_line ( &Out, start_type, points, Cats );

      polyline++;
  }

  G_message (_("The number of polylines in the output map is %d "), polyline);


  /* Tidy up */
  Vect_destroy_line_struct (points);
  Vect_destroy_cats_struct (Cats);
  G_free ( lines_visited );
  Vect_close (&map);
  Vect_build (&Out, stdout); 
  Vect_close (&Out);

  exit (EXIT_SUCCESS);
}



