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


/*****************************************************************************/
/**************************** struct_io.c ************************************/
/*****************************************************************************/

int 
dig_Rd_P_node (
		struct Map_info *map,
		struct P_node *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_node (map, ptr, fp);
}

int 
dig_Wr_P_node (
		struct Map_info *map,
		struct P_node *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_node (map, ptr, fp);
}

int 
dig_Rd_P_line (
		struct Map_info *map,
		struct P_line *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_line (map, ptr, fp);
}

int 
dig_Wr_P_line (
		struct Map_info *map,
		struct P_line *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_line (map, ptr, fp);
}

int 
dig_Rd_P_area (
		struct Map_info *map,
		struct P_area *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_area (map, ptr, fp);
}

int 
dig_Wr_P_area (
		struct Map_info *map,
		struct P_area *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_area (map, ptr, fp);
}

/* island stuff */
int 
dig_Rd_P_isle (
		struct Map_info *map,
		struct P_isle *ptr,
		FILE * fp)
{
  return dig_x_Rd_P_isle (map, ptr, fp);
}

int 
dig_Wr_P_isle (
		struct Map_info *map,
		struct P_isle *ptr,
		FILE * fp)
{
  return dig_x_Wr_P_isle (map, ptr, fp);
}

int 
dig_Rd_P_att (
	       struct Map_info *map,
	       struct P_att *ptr,
	       FILE * fp)
{
  return dig_x_Rd_P_att (map, ptr, fp);
}

int 
dig_Wr_P_att (
	       struct Map_info *map,
	       struct P_att *ptr,
	       FILE * fp)
{
  return dig_x_Wr_P_att (map, ptr, fp);
}

int 
dig_Rd_Plus_head (
		   struct Map_info *map,
		   struct Plus_head *ptr,
		   FILE * fp)
{
/*
   if (NULL == dig__get_head (map->digit))
   G_fatal_error ("Programmer did not call dig_P_init()");
 */
  return dig_x_Rd_Plus_head (map, ptr, fp);
}

int 
dig_Wr_Plus_head (
		   struct Map_info *map,
		   struct Plus_head *ptr,
		   FILE * fp)
{
  return dig_x_Wr_Plus_head (map, ptr, fp);
}


