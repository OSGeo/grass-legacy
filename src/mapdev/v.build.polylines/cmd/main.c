/***********************************************************************/
/*
  v.build.polylines   in $GIS/src.contrib/MWL

  *****
 
  Mark Lake  5/4/00

  University College London
  Institute of Archaeology
  31-34 Gordon Square
  London.  WC1H 0PY

  Email: mark.lake@ucl.ac.uk

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
#include "global_vars.h"
#include "walk.h"
#include "line_coords.h"

/* plus_t defined in mapdev/diglib/dig_structs.h:23 as 
   typedef int plus_t; */

int main (int argc, char **argv)
{
  FILE *output_fd;
  register plus_t line;
  register P_LINE *line_p;
  struct line_pnts *points;
  struct line_coords *coords;
  
  struct Cell_head region;
  struct Map_info map;
  struct GModule *module;
  struct Option *input;
  struct Option *output;
  struct Option *type;
  struct Flag *copy_atts;
  struct Flag *quietly;
  struct Flag *overwrite;
  struct Flag *asciiout;

  plus_t polyline;
  plus_t *lines_visited;
  plus_t points_in_line;
  plus_t points_in_polyline;
  plus_t start_line;
  plus_t asize = 50;
  plus_t ainc = 50;

  char mapset [50];
  char text [73];
  char ascii_line_type;
  char instruction [512];

  int i;


  /*  Initialize the GIS calls */

  G_gisinit(argv[0]) ;

  module = G_define_module();
  module->description =
	"Build polylines from lines.";

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

  type = G_define_option ();
  type->key = "type";
  type->type = TYPE_STRING;
  type->required = NO;
  type->multiple = NO;
  type->answer = "source";
  type->options = "source,line,area,point";
  type->description = "Set line types to:   \n             source        same as in binary vector map\n             line          lines\n             area          area edges\n             point         points (sites)\n";


  asciiout = G_define_flag ();
  asciiout->key = 'a';
  asciiout->description = "Output file is ASCII map";

  copy_atts = G_define_flag ();
  copy_atts->key = 'c';
  copy_atts->description = "Copy attributes and category files";

  overwrite = G_define_flag ();
  overwrite->key = 'o';
  overwrite->description = "Overwrite output file";

  quietly = G_define_flag ();
  quietly->key = 'q';
  quietly->description = "Run quietly";

  if (G_parser(argc, argv))
    exit (-1);

  
  /* Make parser parameters globally available */
	
  gQuietly = quietly->answer;
  gOverwrite = overwrite->answer;
  gCopy_atts = copy_atts->answer;
  gAsciiout = asciiout->answer;
  if (strcmp (type->answer, "source") == 0)
    gAscii_type = 'S';
  if (strcmp (type->answer, "line") == 0)
    gAscii_type = 'L';
  if (strcmp (type->answer, "area") == 0)
    gAscii_type = 'A';
  if (strcmp (type->answer, "point") == 0)
    gAscii_type = 'P';


  /* Open binary vector file at level 2 */

  strcpy (mapset, G_find_file ("dig", input->answer, ""));
  if (Vect_open_old (&map, input->answer, mapset) < 2)
    G_fatal_error ("Please run v.support");
  

  /* Create ASCII vector file */

  if ((G_find_file ("dig_ascii", output->answer, "") != NULL) ||
      (G_find_file ("dig", output->answer, "") != NULL))
    {
      if (!gOverwrite)
	G_fatal_error ("Output file exists ");
      else
	{
	  if (!gQuietly)
	    fprintf(stderr, "\nOverwriting map %s ", output->answer);
	}
    }
  else
    {
      if (!gQuietly)
	fprintf(stderr, "\nCreating new map %s ", output->answer);
    }
  output_fd = G_fopen_new ("dig_ascii", output->answer);
  if (output_fd == NULL)
    G_fatal_error ("Unable to create output file ");
  

  /* Copy header info. from binary map to ascii map */

  strcpy (text, map.head.organization);
  fprintf (output_fd, "ORGANIZATION: %s", text);
  strcpy (text, map.head.date);
  fprintf (output_fd, "\nDIGIT DATE:   %s", text);
  strcpy (text, map.head.your_name);
  fprintf (output_fd, "\nDIGIT NAME:   %s", text);
  strcpy (text, map.head.map_name);
  fprintf (output_fd, "\nMAP NAME:     %s", text);
  strcpy (text, map.head.source_date);
  fprintf (output_fd, "\nMAP DATE:     %s", text);
  strcpy (text, map.head.line_3);
  fprintf (output_fd, "\nOTHER INFO:   %s", text);
  fprintf (output_fd, "\nMAP SCALE:    %ld", map.head.orig_scale);
  fprintf (output_fd, "\nZONE:         %d", map.head.plani_zone);
  fprintf (output_fd, "\nWEST EDGE:    %f", map.head.W);
  fprintf (output_fd, "\nEAST EDGE:    %f", map.head.E);
  fprintf (output_fd, "\nSOUTH EDGE:   %f", map.head.S);
  fprintf (output_fd, "\nNORTH EDGE:   %f", map.head.N);
  fprintf (output_fd, "\nMAP THRESH:   %f", map.head.map_thresh);
  fprintf (output_fd, "\nVERTI:");


  /* Get the number of lines in the binary map and set up 
     record of lines visited */

  lines_visited = (plus_t*) calloc (map.n_lines + 1, sizeof (plus_t));
  if (lines_visited == NULL)
    G_fatal_error ("Error allocating memory for `lines_visited' ");

  fprintf (stderr, "\n\nThe number of lines in the binary map is %d",
	   map.n_lines);


  /* Set up points structure and coordinate arrays */

  points = Vect_new_line_struct ();
  coords = init_line_coords ();
  if (coords == NULL)
    G_fatal_error ("Unable to allocate memory for `coords' ");


   /* Set ascii line type from global line type */

  ascii_line_type = gAscii_type;


  /* Explicitly set the constraint to include only alive lines */

  Vect_set_constraint_type (&map, (LINE | AREA | DOT));


  /* Step over all lines in binary map */

  polyline = 0;
  if (!reset_line_coords (coords))
    G_fatal_error ("Couldn't reset `coords' ");

  if (!gQuietly)
    fprintf (stderr, "\n");

  while (1)
    {
      line = map.next_line;
      if (line > map.n_lines)
	break;
      line_p = &(map.Line [line]);
      

      /* Skip line if */

      if ((map.Constraint_type_flag && !(line_p->type &
					  map.Constraint_type)))
	{
	  map.next_line++;
	  continue;
	}
      
      
      /* Skip line if already visited from another */
      
      if (lines_visited [line])
	{
	  map.next_line++;
	  continue;
	}
           
     
      /* Only get here if line is alive and not
	 previously visited */

      line = map.next_line;
      polyline ++;
      

      /* Find start of this polyline */
      
      start_line = walk_back (&map, line);
      if (!gQuietly)
	{
	  fprintf (stdout, "\nPolyline %d: start line = %d ",
		   polyline, start_line);
	  fflush (stdout);
	}
      

      /* Walk forward and pick up coordinates */

      points_in_polyline = 
	walk_forward_and_pick_up_coords (&map, start_line, points, coords, 
					 lines_visited, &ascii_line_type);


      /* Write coordinates to ASCII map */
      
      fprintf (output_fd, "\n%c %d", ascii_line_type, points_in_polyline);
      for (i=0; i<points_in_polyline; i++)
	{
	  fprintf (output_fd, "\n %f %f", coords->y[i], coords->x[i]);
	}

      map.next_line++;
    }

  fprintf (stderr, "\n\nThe number of polylines in the output map is %d \n",
	   polyline);


  /* Tidy up */

  Vect_remove_constraints (&map);
  free_line_coords (coords);
  Vect_destroy_line_struct (points);
  Vect_close (&map);
  fclose (output_fd);


  /* Copy dig_atts and dig_cats files if requested and if they exist */

  if (gCopy_atts)
    {
      if (G_find_file ("dig_att", input->answer, mapset) != NULL)
	{
	  fprintf (stderr, "\nCopying attributes ");
	  sprintf (instruction, "cp %s/%s/dig_att/%s %s/%s/dig_att/%s", 
		   G_location_path (), mapset, input->answer,
		   G_location_path (), mapset, output->answer); 
	  G_system (instruction);
	}
      
      if (G_find_file ("dig_cats", input->answer, mapset) != NULL)
	{
	  fprintf (stderr, "\nCopying categories ");
	  sprintf (instruction, "cp %s/%s/dig_cats/%s %s/%s/dig_cats/%s", 
		   G_location_path (), mapset, input->answer,
		   G_location_path (), mapset, output->answer); 
	  G_system (instruction);
	}
    }


  /* Create binary vector map if required */

  if (!gAsciiout)
    {
      fprintf (stderr, "\nCreating binary map ");
      sprintf (instruction, "v.in.ascii input=%s output=%s",
	       output->answer, output->answer);
      G_system (instruction);

      fprintf (stderr, "\nBuilding topology ");
      if (!gQuietly)
	sprintf (instruction, "v.support map=%s option=build",
	       output->answer);
      else
	sprintf (instruction, "v.support map=%s option=build > /dev/null",
		 output->answer);
      G_system (instruction);

      sprintf (instruction, "rm %s/%s/dig_ascii/%s", 
	       G_location_path (), mapset, output->answer);
      G_system (instruction);
    }
  else
    fprintf (stderr, 
	   "\nRun v.import to convert the output map to binary format\n");


  return (0);
}
