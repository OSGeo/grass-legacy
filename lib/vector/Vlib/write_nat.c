/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.1 Radim Blazek and David D. Gray.
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdio.h>
#include "gis.h"
#include "Vect.h"

long V1__rewrite_line_nat ( struct Map_info *Map, long   offset, int    type,
		       struct line_pnts *points, struct line_cats *cats);

/* Writes line to 'coor' file.
*  
*  Returns: offset into file
*           -1 on error */
long 
V1_write_line_nat (
		     struct Map_info *Map,
		     int    type,
		     struct line_pnts *points,
		     struct line_cats *cats)
{
  long offset;

  fseek (Map->dig_fp, 0L, SEEK_END);	/*  end of file */
  offset = ftell (Map->dig_fp);

  return V1__rewrite_line_nat (Map, offset, type, points, cats);
}

/* Rewrites line at the given offset.
*  If the number of points or cats differs from
*  the original one or the type is changed:
*  GV_POINTS -> GV_LINES or GV_LINES -> GV_POINTS,
*  the old one is deleted and the
*  new is appended to the end of the file.
*
*  Returns: line offset
*           -1 on error
*/
long 
V1_rewrite_line_nat (
		       struct Map_info *Map,
		       long   offset,
		       int    type,
		       struct line_pnts *points,
		       struct line_cats *cats)
{
  int    n_points;
  int    old_type, del_type;
  FILE   *dig_fp;
  struct line_pnts *old_points;
  struct line_cats *old_cats; 
  long   new_offset;
 
  /* TODO: enable points and cats == NULL  */
  
  /* First compare numbers of points and cats with tha old one */
  old_points = Vect_new_line_struct ();
  old_cats = Vect_new_cats_struct ();

  old_type = V1_read_line_nat ( Map, old_points, old_cats, offset );
  if ( old_type == -1 ) return (-1); /* error */

  if ( old_type != -2 /* EOF -> write new line */
       && points->n_points == old_points->n_points 
       && cats->n_cats == old_cats->n_cats
       && (   ( (type & GV_POINTS) && (old_type & GV_POINTS) )   
           || ( (type & GV_LINES ) && (old_type & GV_LINES ) ) ) ) {
      /* equal -> overwrite the old */
      return V1__rewrite_line_nat (Map, offset, type, points, cats);
  } else {
      /* differ -> delete the old and append new */
      /* delete old */
      switch (old_type) {
	  case GV_POINT:
	  case GV_DEAD_POINT:
	      del_type = GV_DEAD_POINT;
	      break;
	  case GV_LINE:
	  case GV_DEAD_LINE:
	      del_type = GV_DEAD_LINE;
	      break;
	  case GV_BOUNDARY:
	  case GV_DEAD_BOUNDARY:
	      del_type = GV_DEAD_BOUNDARY;
	      break;
	  case GV_CENTROID:
	  case GV_DEAD_CENTROID:
	      del_type = GV_DEAD_CENTROID;
	      break;
      }
      
      if ( old_type != -2 ) /* EOF -> write new line */
          if (-1 == V1__rewrite_line_nat (Map, offset, del_type, old_points, old_cats) )
              return (-1);
      
      /* write new */
      fseek (Map->dig_fp, 0L, SEEK_END);	/*  end of file */
      new_offset = ftell (Map->dig_fp);

      return V1__rewrite_line_nat (Map, new_offset, type, points, cats);
  }
}

/* Rewrites line at the given offset.
*
*  Returns: line offset
*           -1 on error
*/
long 
V1__rewrite_line_nat (
		       struct Map_info *Map,
		       long   offset,
		       int    type,
		       struct line_pnts *points,
		       struct line_cats *cats)
{
  int  n_points;
  FILE *dig_fp;
  
  dig__set_cur_head (&(Map->head));
  dig_fp = Map->dig_fp;
  fseek (dig_fp, offset, 0);

  if (0 >= dig__fwrite_port_I (&type, 1, dig_fp))
    return -1;

  if (0 >= dig__fwrite_port_C (&cats->n_cats, 1, dig_fp))
    return -1;

  if (cats->n_cats > 0)
    {
      if (0 >= dig__fwrite_port_I (cats->field, cats->n_cats, dig_fp))
	return -1;
      if (0 >= dig__fwrite_port_I (cats->cat, cats->n_cats, dig_fp))
	return -1;
    }
  
  if ( type & ( DOT | DEAD_DOT | CENTROID | DEAD_CENTROID ) )
    n_points = 1;
  else  
    n_points = points->n_points;	  
  
  dig__fwrite_port_I (&n_points, 1, dig_fp);

  
  if (0 >= dig__fwrite_port_D (points->x, n_points, dig_fp))
    return -1;
  if (0 >= dig__fwrite_port_D (points->y, n_points, dig_fp))
    return -1;

  if (Map->head.with_z == WITH_Z)
    {
      if (0 >= dig__fwrite_port_D (points->z, n_points, dig_fp))
          return -1;
    }
  
  fflush (dig_fp);

  return offset;
}

