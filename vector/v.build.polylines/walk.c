#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "walk.h"
#include "global.h"

/* find next line for given line and node 
*  return: next line (may be input line if it is loop)
*          0 - num of lines <> 2
*/
int find_next_line ( struct Map_info *map, int line, int node ) 
{
    int n_lines, i, tmp_line, tmp_type, next_line;
    
    G_debug (2, "  find_next_line() line = %d node = %d", line, node);
    next_line = 0;
    n_lines = 0;
    for ( i = 0; i < Vect_get_node_n_lines ( map, node ); i++ ) {
        tmp_line = abs ( Vect_get_node_line ( map, node, i ) );
        tmp_type = Vect_read_line ( map, NULL, NULL, tmp_line); 
	/* The line may be a loop so we want some other line if exists or the same line if loop */
        if ( tmp_type & GV_LINES ) {
	    if ( next_line == 0 || tmp_line != line ) next_line = tmp_line;
	    n_lines++;
        }
    }
    if ( n_lines != 2 ) next_line = 0;

    G_debug (2, "  -> next line = %d", next_line);
    return next_line;
}

/* Start from some arbitrary line on a polyline and walk back to find
   the first node (i.e. for which the number of connected lines <> 2)
   This line must not be a dead line (note that the arbitrary line
   cannot be a dead line because this has already been checked in
   main.c. */
int walk_back ( struct Map_info *map, int start_line)
{
  int start_node, n1, n2;
  int line;
  int next_line;
  int type;

  G_debug (2, "walk_back() start = %d", start_line);
  line = start_line;

  /* By definition a GV_POINT and GV_CENTROID does not form part of a longer polyline, so
     this line must be the start */
  type = Vect_read_line ( map, NULL, NULL, line);
  if ( type == GV_POINT || type == GV_CENTROID ) return (line);

  /* Otherwise find the start (i.e. travel in the negative direction) */
  Vect_get_line_nodes ( map, line, &start_node, NULL );
  
  while ( 1 ) {
      /* Find next line at start node */
      next_line = find_next_line ( map, line, start_node );
      G_debug (2, "  next = %d", next_line);
	  
      /* Keep going so long as not returned to start_line, i.e. if not a closed set of lines */
      if ( next_line == 0 || next_line == start_line ) break; 

      line = next_line;
      /* In a heavily edited binary vector map the relationship
	 between the direction of a line (in terms of whether it is
	 positive or negative in a node's line array) and the order of
	 the line's nodes N1 and N2 is not constant.  So here we flip
	 the direction of travel if the initial direction of travel
	 points back to the same line. */
      Vect_get_line_nodes ( map, next_line, &n1, &n2 );
      if ( n2 == start_node ) start_node = n1;
      else start_node = n2;
  }
  
  return (line);
}

/* Start from the first node on a polyline and walk to the other end,
   collecting the coordinates of each node en route.  */
int walk_forward_and_pick_up_coords (
     struct Map_info *map,
     int start_line,
     struct line_pnts *points,
     int *lines_visited
)
{
  int line, next_line, n1, n2;
  int type, node, next_node;
  struct line_pnts *pnts;

  G_debug (2, "  walk_forward() start = %d", start_line);
  line = start_line;
  pnts = Vect_new_line_struct ();
  Vect_reset_line ( points );

  /* Pick up first set of coordinates */
  lines_visited [line] = 1;
  type = Vect_read_line (map, pnts, NULL, line);
  
  Vect_get_line_nodes ( map, line, &n1, &n2 );
  next_line = find_next_line ( map, line, n1 );
  if ( next_line > 0 ) {  /* continue at start node */
     Vect_append_points ( points, pnts, GV_BACKWARD );
     next_node = n1;
  } else { 
     Vect_append_points ( points, pnts, GV_FORWARD );
     next_line = find_next_line ( map, line, n2 ); /* check end node */
     if ( next_line > 0 ) { 
         next_node = n2; /* continue at end node */
     } else {
	 return 1; /* no other line */
     }
  }
      
  /* This line can only be part of a longer polyline if it is not a point.  */
  if ( type & GV_POINTS ) return 1;

  /* While next line exist append coordinates */
  line = next_line;
  node = next_node;
  while ( 1 ) {
      G_debug (2, "  line = %d", line);
      type = Vect_read_line (map, pnts, NULL, line);
      Vect_get_line_nodes ( map, line, &n1, &n2 );

      if ( node == n1 ) {
	  Vect_append_points ( points, pnts, GV_FORWARD );
	  next_node = n2;
      } else {
	  Vect_append_points ( points, pnts, GV_BACKWARD );
	  next_node = n1;
      }

      lines_visited [line] = 1;
      
      /* Find next one */
      next_line = find_next_line ( map, line, next_node );
      if ( next_line == 0 || next_line == start_line ) return 1;
      
      line = next_line;
      node = next_node;
  }
 
  return 1;
}

