#include <stdlib.h>
#include "Vect.h"
/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

/*
** build_area_with_line ()
** this is the real guts of build area stuff
**
** Area is built in clockwise order.
** Take a given line and start off to the RIGHT/LEFT and try to complete
** an area. 
** 
** Possible Scenarios:
** I.    path runs into first line.                              : AREA!
** II.   path runs into a dead end (no other area lines at node) : no area
** III.  path runs into a previous line that is not 1st line    
**                  or to 1st line but not to start node         : no area
**
** after we find an area then we call point_in_area to see if the
** specified point is w/in the area
**
** old returns  -1:  error   0:  no area    (1:  point in area)
**          -2: island  !!
**
** returns  -1:  error   
**           0:  no area
**           number  of lines
**
*/
int 
dig_build_area_with_line ( struct Plus_head *plus, 
	plus_t first_line, /* always positive */
	int side,          /* side of line to build area on */
	plus_t **lines)    /* pointer to array of lines */
{
  register int i;
  int prev_line, next_line;
  double totalarea;
  static plus_t *array;
  char *p;
  static int array_size;	/* 0 on startup */
  int n_lines;

  if (array_size == 0)		/* first time */
    {
      array_size = 1000;
      array = (plus_t *) dig__falloc (array_size, sizeof (plus_t));
      if (array == NULL )
	return (dig_out_of_memory ());
    }
#ifdef GDEBUG
  G_debug (3, "BUILD_AREA: abs(first_line) = %d, side = %d", first_line, side);
#endif
  if (side == GV_LEFT) {
      first_line  = -first_line;  /* start at node1, reverse direction */
  }
  array[0]  =   first_line;
  prev_line =  -first_line;  /* start at node2 for direct and node1 for
			        reverse direction */
  n_lines = 1;
  while (1)
    {
      next_line = dig_angle_next_line (plus, prev_line, GV_RIGHT, GV_BOUNDARY );
#ifdef GDEBUG
      G_debug (3, "next_line = %d", next_line );
#endif
      if (next_line == 0)
	return (-1);

      /*  I. Area closed. This also handles the problem w/ 1 single area line */
      if (first_line == next_line) {
	  /* GOT ONE!  fill area struct  and return */
	  G_debug (3, "GOT ONE! :");

	  for (i = 0; i < n_lines; i++) {
	      G_debug (3, " area line (%d) = %d", i, array[i]);
	    }

	   *lines = array;
	   return (n_lines);
	}

      /* II. Note this is a dead end */
      /* ( if prev_line != -first_line so it goes after the previous test) ? */
      if (prev_line == next_line)
	{
#ifdef GDEBUG
	  G_debug (3, "Dead_end:");
#endif
	  return (0);		/* dead end */
	}

      /* III. Unclosed ?, I would say started from free end */
      for (i = 0; i < n_lines; i++)
	if (abs (next_line) == abs (array[i]))
	  {
#ifdef GDEBUG
	    G_debug (3, "Unclosed area:");
#endif
	    return (0);		/* ran into a different area */
	  }

      /* otherwise keep going */
      if (n_lines >= array_size)
	{
	  p = dig__frealloc (array, array_size + 100, sizeof (plus_t), array_size);
	  if (p == NULL)
	    return (dig_out_of_memory ());
	  array = (plus_t *) p;
	  array_size += 100;
	}
      array[n_lines++] = next_line;
      prev_line = -next_line;
    }

  return 0;
}

/* dig_add_area
*  Allocate space for new area and create boundary info from array.
*  Then for each line in area, update line (right,left) info.
*
*  Neither islands nor centroids area filled.
*
*  Returns: number of new area
*           -1 error
*/
int 
dig_add_area (struct Plus_head *plus, 
	int n_lines,       /* number of lines */
	plus_t *lines )    /* array of lines, negative for reverse direction */ 
{
    register int i;
    register int area, line;
    P_AREA *Area;
    P_LINE *Line;
    char *p;

#ifdef GDEBUG
    G_debug (3, "dig_add_area():");
#endif
    /* First look if we have space in array of pointers to areas
    *  and reallocate if necessary */
    if ( plus->n_areas >= plus->alloc_areas ) { /* array is full */
	if ( dig_alloc_areas(plus,1000) == -1 )
            return -1;
    }

    /* allocate area structure */
    area = plus->n_areas +1;
    Area = dig_alloc_area();
    if (Area == NULL) return -1;

    if ( dig_area_alloc_line (Area, n_lines) == -1 )
        return -1;
    
    for (i = 0; i < n_lines; i++) {
        line = lines[i];
        Area->lines[i] = line;	
        Line = plus->Line[abs(line)];
        if (line < 0) { /* revers direction -> area on left */
	    if ( Line->left != 0 )
	        G_warning ("Line %d already had area/isle %d to left.", line, Line->left);
	    
	    G_debug (3, "Line %d set left to %d.", line, area);
	    Line->left = area;
	} else {
	    if ( Line->right != 0 )
	        G_warning ("Line %d already had area/isle %d to right.", line, Line->right);
	    
	    G_debug (3, "Line %d set right to %d.", line, area);
	    Line->right = area;
	}
    }
    Area->n_lines = n_lines;
  
    plus->Area[area] = Area;
    plus->n_areas++;
  
    return (area);
}



/* 
** angle_next_line ()
**   assume that lines are sorted in increasing angle order
**
**   Return line number of next angle to follow an line
**                     (negative if connected by node2)
**          0 on error or not found
*/
int 
dig_angle_next_line ( 
      struct Plus_head *plus,
      plus_t current_line,	/* current line number,
                                 * negative if request for node 2 */
      int side,                 /* GV_RIGHT or GV_LEFT */
      int type)                 /* type of line GV_LINE, GV_BOUNDARY or both */
{
  register int next;
  register int current;
  int line;
  plus_t node;
  P_NODE *Node;
  P_LINE *Line;

#ifdef GDEBUG
  G_debug (3, "dig_angle_next_line: line = %d, side = %d, type = %d", 
	           current_line, side, type);
#endif
  Line = plus->Line[abs(current_line)];
  if ( current_line > 0 )
    node = Line->N1;
  else {
    node = Line->N2;
  }
  Node = plus->Node[node];

  /* first find index for that line */
  next = -1;
  for (current = 0; current < Node->n_lines; current++) {
    if (Node->lines[current] == current_line)
      next = current;
  }
  if ( next == -1 ) return 0;       /* not found */
  
  while (1) {
      if (side == GV_RIGHT) {  /* go up (bigger angle) */
	  if (next == Node->n_lines - 1)
	    next = 0;
	  else
	    next++;
      } else {                      /* go down (smaller angle) */
	  if (next == 0)
	    next = Node->n_lines - 1;
	  else
	    next--;
      }
      line = abs ( Node->lines[next] );
      Line = plus->Line[line];
      if ( Line->type == GV_BOUNDARY ) {
	  return ( Node->lines[next] );
      }
  }
#ifdef GDEBUG
  G_debug (3, "Angle_next_line:  Line NOT found at node %d\n", (int) node);
#endif
}


/*   AREAS:

   ///
   Areas exist only if labeled.
   If label is removed, area is removed.

   no longer true.  area are built by import  but can be destroyed
   by editing
   ///

   If line is removed that borders 1 or more areas, those areas are removed.
   Snapping lines CAN cause the same effect (snapping a line that was a border)
   as well as snapping to an area, in some cases.
   Digitizing a new AREA-border that snaps to an area;
   Moving points (END POINTS) CAN cause the same (if such is supported)

   User: Choose 'label areas'
   User: specify label number

   User: Place point w/in area
   User: Select a line on area border

   Program:

   check if in existing area!
   CREATE up to 2 areas w/ line.
   for each area
   Pass line from point thru area line segments
   if (n_intersections  is odd)
   have area
   if (have area)
   build new area info
   allocate space for new area
   copy line info to area
   for each line set area info in line struct
   new_att()
 */

/* dig_add_isle
*  Allocate space for new island and create boundary info from array.
*  The order of impult lines is expected to be counter clockwise.
*  Then for each line in isle, update line (right,left) info.
*
*  Area number the island is within is not filled.
*
*  Returns: number of new isle
*           -1 error
*/
int 
dig_add_isle (struct Plus_head *plus, 
	int n_lines,       /* number of lines */
	plus_t *lines )    /* array of lines, negative for reverse direction */ 
{
    register int i;
    register int isle, line;
    P_ISLE *Isle;
    P_LINE *Line;
    char *p;

#ifdef GDEBUG
    G_debug (3, "dig_add_isle():");
#endif
    /* First look if we have space in array of pointers to isles
    *  and reallocate if necessary */
    if ( plus->n_isles >= plus->alloc_isles ) { /* array is full */
	if ( dig_alloc_isles(plus,1000) == -1 )
	     return -1;
    }

    /* allocate isle structure */
    isle = plus->n_isles + 1;
    Isle = dig_alloc_isle();
    if (Isle == NULL) return -1;

    if ( ( dig_isle_alloc_line (Isle, n_lines) ) == -1 )
        return -1;

    Isle->area = -1;

    Isle->N = 0;
    Isle->S = 0;
    Isle->E = 0;
    Isle->W = 0;
    
    for (i = 0; i < n_lines; i++) {
        line = lines[i];
        G_debug (3, " i = %d line = %d", i, line);
        Isle->lines[i] = line;	
        Line = plus->Line[abs(line)];
        if (line < 0) { /* revers direction -> isle on left */
	    if ( Line->left != 0 )
	        G_warning ("Line %d already had area/isle %d to left.", line, Line->left);
	    
	    Line->left = -isle;
	} else {
	    if ( Line->right != 0 )
	        G_warning ("Line %d already had area/isle %d to left.", line, Line->right);
	    
	    Line->right = -isle;
	}
    }

    Isle->n_lines = n_lines;
  
    plus->Isle[isle] = Isle;
    plus->n_isles++;
  
    return (isle);
}


/*
   ** 
   **  mark isle as deleted,  delete its attribute 
   **  unmark references to it in Line array
 */
/*
int 
dig_del_isle (struct Map_info *map, int isle)
{
  register int i, line;
  P_ISLE *Isle;

  isle = abs (isle);
  Isle = &(map->Isle[isle]);

  for (i = 0; i < Isle->n_lines; i++)	
    {
      line = Isle->lines[i];
      if (line < 0)		
	{
	  map->Line[abs (line)].left = 0;
	}
      else
	map->Line[line].right = 0;
    }
  if (Isle->alloc_lines)
    {
      free (Isle->lines);
    }
  Isle->alloc_lines = 0;
  Isle->n_lines = 0;

  Isle->alive = 0;	

  return (0);
}
*/
