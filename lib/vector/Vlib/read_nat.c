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
/*
 *V1_read_next_line (Map, line_p)
 *              reads thru digit file looking for next line within window,
 *              stores info in struct instead of args.
 *      NOTE:  line_p->alloc_points better be set to 0 for the first call.
 * 
 * returns:  -1 on error
 *           -2 EOF
 *           line type (positive value) if it found a line

 **
 **
 **  The action of this routine can be modified by:
 **    Vect_read_constraint_region ()
 **    Vect_read_constraint_type   ()
 **    Vect_remove_constraints     ()
 **
 */

#include "gis.h"
#include "Vect.h"

/*
*  Read line from coor file on given offset.
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*/
int 
V1_read_line_nat (
	       struct Map_info *Map,
	       struct line_pnts *Points,
	       struct line_cats *Cats,
	       long offset)
{
  return Vect__Read_line_nat (Map, Points, Cats, offset);
}

/*
*  Read next line from coor file.
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*/
int 
V1_read_next_line_nat (
		    struct Map_info *Map,
		    struct line_pnts *line_p,
		    struct line_cats *line_c)
{
  int itype;

  double n, s;
  double e, w;

  while (1)
    {
      itype = Vect__Read_line_nat (Map, line_p, line_c, ftell (Map->dig_fp));
      if (itype < 0)
	return (itype);

      /* Constraint on Type of line 
       * Default is all of  Point, Line, Area and whatever else comes along
       */
      if (Map->Constraint_type_flag)
	{
	  if (!(itype & Map->Constraint_type))
	    continue;
	}
      else
	{
	  /* if (!LINE_ALIVE ()) */
	  if (itype & ELEMENT_TYPE_DEAD)	/* is it DEAD? */
	    continue;
	}

/*  calculate the bounding box for the line  */
      /* 4.0 dig_bound_box2() needs a scale to figure fudge factor
         **   I am not concered about fudge here, so just take 
         **   any number.  I picked 16000 cuz that is the default
         **   in dig_bound_box2() and thus faster.
       */
      /*
         **  Constraint on specified region
       */
      if (Map->Constraint_region_flag)
	{
	  dig_bound_box2 (line_p, &n, &s, &e, &w, 16000L);	/*4.0 */

	  if (!V__map_overlap (Map, n, s, e, w))
	    continue;
	}

      return (itype);
    }
  /* NOTREACHED */

}				/*  dig_read_line_struct_in_box()  */

/*
   ** reads any specified line   This is NOT affected by constraints
 */
int 
V2_read_line_nat (
	       struct Map_info *Map,
	       struct line_pnts *line_p,
	       struct line_cats *line_c,
	       int line)
{
  if (line < 1 || line > Map->n_lines)	/* ALL DONE */
    return -2;

  return Vect__Read_line_nat (Map, line_p, line_c, Map->Line[line].offset);
}

/* reads next unread line each time called.  use Vect_rewind to reset */
/* returns -2 on end of lines */

int 
V2_read_next_line_nat (
		    struct Map_info *Map,
		    struct line_pnts *line_p,
		    struct line_cats *line_c)
{
  register int line;
  register P_LINE *Line;

  while (1)
    {
      line = Map->next_line;

      if (line > Map->n_lines)
	return (-2);

      Line = &(Map->Line[line]);

      if ((Map->Constraint_type_flag && !(Line->type & Map->Constraint_type)))
	{
	  Map->next_line++;
	  continue;
	}

      if (Map->Constraint_region_flag)
	if (!V__map_overlap (Map, Line->N, Line->S, Line->E, Line->W))
	  {
	    Map->next_line++;
	    continue;
	  }

      return V2_read_line_nat (Map, line_p, line_c, Map->next_line++);
    }
  /* NOTREACHED */
}


/*  
*  read line from coor file 
*
*  Returns  line type
*           -2 End of file
*           -1 Out of memory
*/
int
Vect__Read_line_nat (
		    struct Map_info *Map,
		    struct line_pnts *p,
		    struct line_cats *c,
		    long offset)
{
  int n_points, size;
  GRASS_V_NCATS n_cats;
  int type;

  /* reads must set in_head, but writes use default */
  dig__set_cur_head (&(Map->head));

  fseek (Map->dig_fp, offset, 0);

  if (0 >= dig__fread_port_I (&type, 1, Map->dig_fp))
      return (-2);
  
  if (0 >= dig__fread_port_C (&n_cats, 1, Map->dig_fp))
      return (-2);

  if ( c != NULL )
    {	  
      c->n_cats = n_cats;
      if (n_cats > 0)
      {
        if (0 > dig_alloc_cats (c, (int) n_cats + 1))
	  return (-1);

        if (0 >= dig__fread_port_I (c->field, n_cats, Map->dig_fp))
            return (-2);
        if (0 >= dig__fread_port_I (c->cat, n_cats, Map->dig_fp))
            return (-2);
      }
    }
  else
    {
      size = (PORT_SHORT + PORT_LONG) * n_cats;
      fseek (Map->dig_fp, size, SEEK_CUR);
    }
  
  if (0 >= dig__fread_port_I (&n_points, 1, Map->dig_fp))
      return (-2);

  if (0 > dig_alloc_points (p, (int) n_points + 1))
    {
#ifdef GDEBUG
      G_debug (3, "Vect__Read_line_nat: offset = %ld, type = %d,  npoints = %d\n",
	        offset, type, n_points);
#endif
      return (-1);
    }

  p->n_points = n_points;
  if (0 >= dig__fread_port_D (p->x, n_points, Map->dig_fp))
      return (-2);
  if (0 >= dig__fread_port_D (p->y, n_points, Map->dig_fp))
      return (-2);

  if (Map->head.with_z)
    {
      if (0 >= dig__fread_port_D (p->z, n_points, Map->dig_fp))
          return (-2);
    }
  
  return (type);
}

