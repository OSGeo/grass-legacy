/***********************************************************************/
/*
  v.build.polylines

  *****
 
  Mark Lake  5/4/00

  University College London
  Institute of Archaeology
  31-34 Gordon Square
  London.  WC1H 0PY

  Email: mark.lake@ucl.ac.uk

  Updated to grass 5.1 by Radim Blazek
  
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
#include "gis.h"
#include "Vect.h"
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
  /* struct Flag *overwrite; */

  int polyline;
  int *lines_visited;
  int points_in_polyline;
  int start_line;

  char *mapset;
  int start_type;

  /*  Initialize the GIS calls */
  G_gisinit(argv[0]) ;

  module = G_define_module();
  module->description = "Build polylines from lines.";

  /* Define the options */

  input = G_define_option ();
  input->key = "input";
  input->type = TYPE_STRING;
  input->required = YES;
  input->multiple = NO;
  input->gisprompt = "old,dig,vector";
  input->description = "Input binary vector map";

  output = G_define_option ();
  output->key = "output";
  output->type = TYPE_STRING;
  output->required = YES;
  output->multiple = NO;
  output->gisprompt = "new,dig,vector";
  output->description = "Output vector map";

  /*
  overwrite = G_define_flag ();
  overwrite->key = 'o';
  overwrite->description = "Overwrite output file";
  */

  quietly = G_define_flag ();
  quietly->key = 'q';
  quietly->description = "Run quietly";

  if (G_parser(argc, argv)) exit (-1);
  
  /* Make parser parameters globally available */
  gQuietly = quietly->answer;
  /* gOverwrite = overwrite->answer; */

  /* Open binary vector file at level 2 */
  mapset = G_find_vector2 (input->answer, "");
  if ( mapset == NULL ) G_fatal_error ( "Could not find input %s\n", input->answer);  
  Vect_set_open_level (2);
  Vect_open_old (&map, input->answer, mapset);

  /* Open new vector */
  if ( G_find_vector2 ( output->answer, "") != NULL ) {
      /*
      if (!gOverwrite)
	G_fatal_error ("Output file exists ");
      else {
      */
	  if (!gQuietly)
	    fprintf(stderr, "\nOverwriting map %s ", output->answer);
      /*
      }
      */
  } else {
      if (!gQuietly)
	fprintf(stderr, "\nCreating new map %s ", output->answer);
  }
  Vect_open_new ( &Out, output->answer, Vect_is_3d(&map) );

  /* Copy header info. */
  Vect_copy_head_data (&map, &Out); 

  /* Get the number of lines in the binary map and set up record of lines visited */

  lines_visited = (int*) G_calloc ( Vect_get_num_lines(&map) + 1, sizeof (int));
  if (lines_visited == NULL) G_fatal_error ("Error allocating memory for `lines_visited' ");

  fprintf (stderr, "\n\nThe number of lines in the binary map is %d", Vect_get_num_lines(&map) );

  /* Set up points structure and coordinate arrays */
  points = Vect_new_line_struct ();
  Cats = Vect_new_cats_struct ();

  /* Step over all lines in binary map */
  polyline = 0;

  if (!gQuietly) fprintf (stderr, "\n");

  for ( line = 1; line < Vect_get_num_lines(&map); line++ ) {
      /* Skip line if already visited from another */
      if (lines_visited [line]) continue;
     
      /* Only get here if line is not previously visited */
      
      /* Find start of this polyline */
      start_line = walk_back (&map, line);
      start_type = Vect_read_line ( &map, NULL, NULL, start_line );
      if (!gQuietly) {
	  fprintf (stdout, "\nPolyline %d: start line = %d ", polyline, start_line);
	  fflush (stdout);
      }

      /* Walk forward and pick up coordinates */
      points_in_polyline = walk_forward_and_pick_up_coords (&map, start_line, points, lines_visited);

      /* Write the line (type of the first line is used) */
      Vect_write_line ( &Out, start_type, points, Cats );

      polyline++;
  }

  fprintf (stderr, "\n\nThe number of polylines in the output map is %d \n", polyline);


  /* Tidy up */
  Vect_destroy_line_struct (points);
  Vect_destroy_cats_struct (Cats);
  free ( lines_visited );
  Vect_close (&map);
  Vect_build (&Out, stdout); 
  Vect_close (&Out);

  return (0);
}
