#include <string.h>
#include "Vect.h"

/* for dig_globs.h */
#define CONTROL			/* for Memory_io stuff */



/**********************************************************/
/******************** point_io.c **************************/
/**********************************************************/

/*  Read_line ()
   **     read line info from digit file into line_points structure 
   **
   **  Returns     (int)  type  or
   **    -2  End of file
   **   -1 Out of memory

   **  if Line_In_Memory is TRUE, then offset is taken off of Mem_Line_Ptr
   **  in memory.
 */


int 
Vect__Read_line (
		  struct Map_info *Map,
		  struct line_pnts *p,
		  struct line_cats *c,
		  long offset)
{
  return Vect_x__Read_line (Map, p, c, offset);
}

/* write line info to DIGIT file */
/*  returns offset into file */
long 
Vect__Write_line (
		   struct Map_info *Map,
		   int type,
		   struct line_pnts *points,
		   struct line_cats *cats)
{
  return Vect_x__Write_line (Map, type, points, cats);
}

/* write line info to DIGIT file */
/*  at the given offset */
/*  obviously the number of points must NOT have changed */
/*  from when line was read in */

int 
Vect__Rewrite_line (
		     struct Map_info *Map,
		     long offset,
		     int type,
		     struct line_pnts *points,
		     struct line_cats *cats)
{
  return Vect_x__Rewrite_line (Map, offset, type, points, cats);
}


