/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.7 Radim Blazek and David D. Gray.
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

int Vect__Read_line_nat (struct Map_info *, struct line_pnts *, struct line_cats *, long);

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
  long offset;
  BOUND_BOX lbox, mbox;

  G_debug (3, "V1_read_next_line_nat()" );

  if (Map->Constraint_region_flag)
      Vect_get_constraint_box ( Map, &mbox );

  while (1)
    {
      offset = dig_ftell ( &(Map->dig_fp) );
      itype = Vect__Read_line_nat (Map, line_p, line_c, offset);
      if (itype < 0)
	return (itype);

      if (itype == 0)	/* is it DEAD? */
	continue;

      /* Constraint on Type of line 
       * Default is all of  Point, Line, Area and whatever else comes along
       */
      if (Map->Constraint_type_flag)
	{
	  if (!(itype & Map->Constraint_type))
	    continue;
	}

      /* Constraint on specified region */
      if (Map->Constraint_region_flag) {
	  Vect_line_box ( line_p, &lbox );

	  if ( !Vect_box_overlap (&lbox, &mbox) )
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
    P_LINE *Line;

    G_debug (3, "V2_read_line_nat(): line = %d", line); 
    
    
    Line = Map->plus.Line[line]; 

    if ( Line == NULL )
	G_fatal_error ( "Attempt to read dead line %d", line );
	    
    return Vect__Read_line_nat (Map, line_p, line_c, Line->offset);
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
  BOUND_BOX lbox, mbox;

  G_debug (3, "V2_read_next_line_nat()"); 
  
  if (Map->Constraint_region_flag)
      Vect_get_constraint_box ( Map, &mbox );

  while (1)
    {
      line = Map->next_line;

      if (line > Map->plus.n_lines)
	return (-2);

      Line = Map->plus.Line[line];
      if ( Line == NULL) {           /* Dead line */
	  Map->next_line++;
	  continue;
      }

      if ((Map->Constraint_type_flag && !(Line->type & Map->Constraint_type)))
	{
	  Map->next_line++;
	  continue;
	}

      if (Map->Constraint_region_flag) {
	Vect_get_line_box ( Map, line, &lbox );   
	if ( !Vect_box_overlap (&lbox, &mbox) )
	  {
	    Map->next_line++;
	    continue;
	  }
      }

      return V2_read_line_nat (Map, line_p, line_c, Map->next_line++);
    }

  /* NOTREACHED */
}


/*  
*  read line from coor file 
*
*  Returns:  line type ( > 0 )
*            0 Dead line
*           -1 Out of memory
*           -2 End of file
*/
int
Vect__Read_line_nat (
		    struct Map_info *Map,
		    struct line_pnts *p,
		    struct line_cats *c,
		    long offset)
{
  int i, dead = 0;   
  int n_points, size;
  int n_cats, do_cats;
  int type;
  char rhead, nc;
  short field;

  G_debug (3, "Vect__Read_line_nat: offset = %ld", offset);
 
  Map->head.last_offset = offset;
  
  /* reads must set in_head, but writes use default */
  dig_set_cur_port (&(Map->head.port));

  dig_fseek ( &(Map->dig_fp), offset, 0);

  if (0 >= dig__fread_port_C (&rhead, 1, &(Map->dig_fp) ))
      return (-2);

  if ( !(rhead & 0x01) ) /* dead line */
      dead = 1;
  
  if ( rhead & 0x02 ) /* categories exists */
      do_cats = 1;    /* do not return here let file offset moves forward to next */
  else 		      /* line */
      do_cats = 0;
  
  rhead >>= 2;
  type = dig_type_from_store ( (int) rhead );  
 
  G_debug (3, "    type = %d, do_cats = %d dead = %d", type, do_cats, dead);
	  
  if ( c != NULL )
      c->n_cats = 0;
  
  if ( do_cats ) { 
      if ( Map->head.Version_Minor == 1 ) { /* coor format 5.1 */
	  if (0 >= dig__fread_port_I ( &n_cats, 1, &(Map->dig_fp) )) return (-2);
      } else { /* coor format 5.0 */
	  if (0 >= dig__fread_port_C (&nc, 1, &(Map->dig_fp) )) return (-2);
	  n_cats = (int) nc;
      }
      G_debug (3, "    n_cats = %d", n_cats);
      
      if ( c != NULL )
	{	  
	  c->n_cats = n_cats;
	  if (n_cats > 0)
	  {
	    if (0 > dig_alloc_cats (c, (int) n_cats + 1))
	      return (-1);
            
            if ( Map->head.Version_Minor == 1 ) { /* coor format 5.1 */
	        if (0 >= dig__fread_port_I (c->field, n_cats, &(Map->dig_fp) )) return (-2);
	    } else { /* coor format 5.0 */
		for (i = 0; i < n_cats; i++) { 
		    if (0 >= dig__fread_port_S (&field, 1, &(Map->dig_fp) )) return (-2);
		    c->field[i] = (int) field; 
		}
	    }
	    if (0 >= dig__fread_port_I (c->cat, n_cats, &(Map->dig_fp) )) return (-2);
	    
	  }
	}
      else
	{
          if ( Map->head.Version_Minor == 1 ) { /* coor format 5.1 */
	      size = ( 2 * PORT_INT ) * n_cats;
	  } else { /* coor format 5.0 */
	      size = ( PORT_SHORT + PORT_INT ) * n_cats;
	  }
	      
	  dig_fseek ( &(Map->dig_fp), size, SEEK_CUR);
	}
  }
  
  if ( type & GV_POINTS ) {
      n_points = 1;
  } else {
      if (0 >= dig__fread_port_I (&n_points, 1, &(Map->dig_fp) )) return (-2);
  }

#ifdef GDEBUG
  G_debug (3, "    n_points = %d", n_points);
#endif

  if ( p != NULL ) {	  
      if (0 > dig_alloc_points (p, n_points + 1))
	  return (-1);

      p->n_points = n_points;
      if (0 >= dig__fread_port_D (p->x, n_points, &(Map->dig_fp) )) return (-2);
      if (0 >= dig__fread_port_D (p->y, n_points, &(Map->dig_fp) )) return (-2);

      if (Map->head.with_z) {
	  if (0 >= dig__fread_port_D (p->z, n_points, &(Map->dig_fp) )) return (-2);
      } else {
	  for ( i = 0; i <  n_points; i++ )
	      p->z[i] = 0.0;
      }
  } else {
      if (Map->head.with_z) 
	  size = n_points * 3 * PORT_DOUBLE;
      else 
	  size = n_points * 2 * PORT_DOUBLE;
      
      dig_fseek ( &(Map->dig_fp), size, SEEK_CUR);
  }
  
  G_debug (3, "    off = %ld", dig_ftell( &(Map->dig_fp) ));
  
  if ( dead ) return 0;
  
  return (type);
}

