#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "walk.h"
#include "global_vars.h"
#include "local_dig.h"

/* Start from some arbitrary line on a polyline and walk back to find
   the first node (i.e. for which the number of connected lines <> 2)
   This line must not be a dead line (note that the arbitrary line
   cannot be a dead line because this has already been checked in
   main.c. */

plus_t walk_back ( struct Map_info *map, plus_t start_line)
{
  plus_t start_node;
  plus_t next_start_node;
  plus_t line;
  plus_t next_line;
  char type;


  line = start_line;


  /* By definition a DOT does not form part of a longer polyline, so
     this line must be the start */

 if (map->Line [line].type == DOT)
   return (line);


 /* Otherwise find the start (i.e. travel in the negative direction) */

  start_node = map->Line [line].N1;
  while (map->Node [start_node].n_lines == 2)
    {
      next_line = abs (map->Node [start_node].lines [0]);


      /* In a heavily edited binary vector map the relationship
	 between the direction of a line (in terms of whether it is
	 positive or negative in a node's line array) and the order of
	 the line's nodes N1 and N2 is not constant.  So here we flip
	 the direction of travel if the initial direction of travel
	 points back to the same line.  N.B., this is only safe for
	 nodes that have two attached lines. */

      if (next_line == line)
	next_line = abs (map->Node [start_node].lines [1]);
      if ((next_line == 0) || (next_line > map->n_lines))
	G_fatal_error ("Start of line apparently unbounded ");
      type = map->Line [next_line].type;

      
      /* Keep going if next line is not deleted */

      if (type & map->Constraint_type)
	{
	  next_start_node = map->Line [next_line].N1;
	  if (next_start_node == start_node)
	    next_start_node = map->Line [next_line].N2;
	  
	  /* Keep going if not a closed polyline.  Note than the number 
	     lines attached to the one node of a single closed polyline
	     is 2 (not sure why, but that's the way it is), so we check
	     that node N1 is not same as node N2. */

	  if (map->Line [next_line].N1 !=  map->Line [next_line].N2)
	    {


	      /* Keep going so long as not returned to start_line, i.e. if
		 not a closed set of lines */

	      if (next_line != start_line)
		{
		  line = next_line;
		  start_node = next_start_node;
		}


	  /* Else use previous line as start line */

	      else
		break;
	    }
	  else 
	    break;
	}
      else
	break;
    }
  
  return (line);
}





/* Start from the first node on a polyline and walk to the other end,
   collecting the coordinates of each node en route.  Check for deleted
   lines en route (note that the start line
   cannot be a dead line because this has already been checked in
   walk_back ()). */

plus_t walk_forward_and_pick_up_coords (
     struct Map_info *map,
     plus_t start_line,
     struct line_pnts *points,
     struct line_coords *coords,
     plus_t *lines_visited,
     char *ascii_line_type
)
{
  char type;
  char next_line_type;
  plus_t line;
  plus_t next_line;
  plus_t no_coords;
  plus_t coords_this_line;
  plus_t end_node;
  plus_t next_end_node;
  plus_t n = 1;       /* Forces Vect_copy_pnts_to_xy to copy 1 point,
			 that function then internally sets n to the
			 actual number of points */
  double *x, *y;
  int i;

  line = start_line;

  
  /* Pick up type of first line */
  
  type = map->Line [line].type;


  /* Pick up first set of coordinates */

  lines_visited [line] = 1;
  no_coords = v2_get_no_line_points (map, line) - 1;
  if (!resize_line_coords (coords, no_coords))
    G_fatal_error ("Out of memory while resizing `coords' ");
  if (0 > V2_read_line (map, points, line))
    G_fatal_error ("Failed to read line ");
  n = Vect_copy_pnts_to_xy (points, &coords->x[0], &coords->y[0], &n);


  /* This line can only be part of a longer polyline if it is not a
     DOT.  */

  if (map->Line [line].type != DOT)
    {


      /* Pick up additional types and coordinates */
      
      end_node = map->Line [line].N2;
     
      
      /* If already at end of line, try going the other way */

      if (map->Node [end_node].n_lines != 2){
	end_node = map->Line [line].N1;
	if(!(x=(double *) malloc(n * sizeof(double))))
		G_fatal_error ("Unable to allocate memory for `x'");
	if(!(y=(double *) malloc(n * sizeof(double))))
		G_fatal_error ("Unable to allocate memory for `y'");
	for(i=0; i<n; i++){
		x[i] = coords->x[n-1-i];
		y[i] = coords->y[n-1-i];
	}
	for(i=0; i<n; i++){
		coords->x[i] = x[i];
		coords->y[i] = y[i];
	}
	free(x);
	free(y);
      }

      while (map->Node [end_node].n_lines == 2)
	{
	  next_line = abs (map->Node [end_node].lines [0]);


	  /* In a heavily edited binary vector map the relationship
	     between the direction of a line (in terms of whether it
	     is positive or negative in a node's line array) and the
	     order of the line's nodes N1 and N2 is not constant.  So
	     here we flip the direction of travel if the initial
	     direction of travel points back to the same line.  N.B.,
	     this is only safe for nodes that have two attached
	     lines. */
	  
	  if (next_line == line)
	    next_line = abs (map->Node [end_node].lines [1]);
	  if ((next_line == 0) || (next_line > map->n_lines))
	    G_fatal_error ("End of line apparently unbounded ");
	  next_line_type = map->Line [next_line].type;

	  
	  /* Keep going if next line is not deleted */
	  
	  if (next_line_type & map->Constraint_type)
	    {
	      next_end_node = map->Line [next_line].N2;
	      if (next_end_node == end_node)
		next_end_node = map->Line [next_line].N1;

	  /* Keep going if not a closed polyline.  Note than the number 
	     lines attached to the one node of a single closed polyline
	     is 2 (not sure why, but that's the way it is), so we check
	     that next end node is not same as previous one. */

	      if (map->Line [next_line].N1 !=  map->Line [next_line].N2)
		{	


		  /* Keep going so long as not returned to start_line, i.e. if
		     not a closed set of lines */

		  if (next_line != start_line)
		    {		  
		      line = next_line;
		      end_node = next_end_node;
		    }


		  /* Else use previous line as end line */

		  else
		    break;
		}
	      else
		break;
	    }
	  else
	    break;
	  
	  
	  /* Warn if polyline contains lines of mixed type */
	  
	  if (map->Line [line].type != type)
	    {
	      if (!gQuietly)
		{
		  fprintf (stdout, 
			   "\tWARNING: contains lines of mixed type");
		  fflush (stdout);
		}
	    }
	  
	  
	  /* Pick up coordinates */
	  
	  Vect_reset_line (points);
	  n = 1;
	  coords_this_line = v2_get_no_line_points (map, line);
	  if (!resize_line_coords (coords, no_coords + coords_this_line))
	    G_fatal_error ("Out of memory while resizing `coords' ");
	  if (0 > V2_read_line (map, points, line))
	    G_fatal_error ("Failed to read line ");
	  
	  
	  /* Overwrite end point of previous line with start point
	     of this, since they should be the same */
	  
	  Vect_copy_pnts_to_xy (points, &coords->x[no_coords],
				&coords->y[no_coords], &n);

	  if(end_node != map->Line [line].N2){
	    if(!(x=(double *) malloc(n * sizeof(double))))
		G_fatal_error ("Unable to allocate memory for `x'");
	    if(!(y=(double *) malloc(n * sizeof(double))))
		G_fatal_error ("Unable to allocate memory for `y'");
	    for(i=0; i<n; i++){
		x[i] = coords->x[no_coords+n-1-i];
		y[i] = coords->y[no_coords+n-1-i];
	    }
	    for(i=0; i<n; i++){
		coords->x[no_coords+i] = x[i];
		coords->y[no_coords+i] = y[i];
	    }
	    free(x);
	    free(y);
	  }
	  
	  no_coords += coords_this_line - 1;

	  /* Record visit to line so that it can be skipped when 
	     looping through all lines in the binary map  */
	  
	  lines_visited [line] = 1;
	}
    }
  no_coords ++;

 
  /* If required, set line type to type of first line */

  if (gAscii_type == 'S')
    {
      switch (type) 
	{
	  /* Treat points and dots as same, since v.in.ascii only
	     recognises P as a type specifier */
	  
	case (LINE): *ascii_line_type = 'L'; break;
	case (AREA): *ascii_line_type = 'A'; break;
	case (DOT): *ascii_line_type = 'P'; break;
	case (POINT): *ascii_line_type = 'P'; break;
	  
	  /* Shouldn't ever get to pick up dead lines, but just in case */
	  
	case (DEAD_LINE): *ascii_line_type = 'l'; break;
	case (DEAD_AREA): *ascii_line_type = 'a'; break;
	case (DEAD_DOT): *ascii_line_type = 'p'; break;
	default: if (!gQuietly)
	  {
	    fprintf (stdout, 
			  "\tWARNING: line of unknown type set to `line'");
	    fflush (stdout);
	  }
	*ascii_line_type = 'L';
	}
    }
  return (no_coords);
}
