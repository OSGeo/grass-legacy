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
V1_read_line_shp (
	       struct Map_info *Map,
	       struct line_pnts *Points,
	       struct line_cats *Cats,
	       long offset)
{
  return Vect__Read_line_shp (Map, Points, Cats, offset);
}

/*
*  Read nex line from coor file.
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*/
int 
V1_read_next_line_shp (
		    struct Map_info *Map,
		    struct line_pnts *line_p,
		    struct line_cats *line_c)
{
  int itype;
  long offset;

  double n, s;
  double e, w;
 
  offset = ( Map->fInfo.shp.shape << 11 ) | ( Map->fInfo.shp.part & 0x7FF);
  while (1)
    {
      itype = Vect__Read_line_shp (Map, line_p, line_c, offset);
      if (itype < 0)
	return (itype);

      /* Constraint on Type of line 
         **  Default is all of  Point, Line, Area and whatever else comes along
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
*  Read line from coor file on given offset.
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*/
int 
V2_read_line_shp (
	       struct Map_info *Map,
	       struct line_pnts *line_p,
	       struct line_cats *line_c,
	       int line)
{
  if (line < 1 || line > Map->n_lines)	/* ALL DONE */
    return -2;

  return Vect__Read_line_shp (Map, line_p, line_c, Map->Line[line].offset);
}

/*
*  Read nex line from coor file.
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*/
int 
V2_read_next_line_shp (
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

      return V2_read_line_shp (Map, line_p, line_c, Map->next_line++);
    }
  /* NOTREACHED */
}


/*
*  Read line from coor file on given offset.
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*/
int
Vect__Read_line_shp (
		    struct Map_info *Map,
		    struct line_pnts *p,
		    struct line_cats *c,
		    long offset)
{
  int i, n_points, size;
  GRASS_V_NCATS n_cats;
  int type;
  int shape, part;
  int first, last; 
  SHPObject *pShape;
  
  offset = ( Map->fInfo.shp.shape << 11 ) | ( Map->fInfo.shp.part & 0x7FF);
  shape = ( offset >> 11 ) & 0x1FFFFF ;
  part = offset & 0x7FF;
  
  if ( shape >= Map->fInfo.shp.nShapes ) {
      return (-2); /* EOF reached */ 
  }
  
  if ( c != NULL )
    {
      c->n_cats = 1;
      Vect_reset_cats ( c );
      /* TODO set cat to value of selected field */
      Vect_cat_set ( c, 1, shape + 1 );
    }
 
  pShape = SHPReadObject( Map->fInfo.shp.hShp, shape ); 
  
  Vect_reset_line ( p ); 
 
  if (  Map->fInfo.shp.type == SHPT_POINT || Map->fInfo.shp.type == SHPT_POINTZ
        || Map->fInfo.shp.type == SHPT_MULTIPOINTM ) {
      first = 0; last = 0;
  } else {
      first = pShape->panPartStart[part];
      if( part == pShape->nParts - 1 ) {
          last = pShape->nVertices - 1;
      } else {
          last = pShape->panPartStart[part+1] - 1;
      }
  }

  for ( i = first; i <= last; i++ ) {
      /* TODO do it better (speed) */ 
      Vect_append_point ( p,  pShape->padfX[i], pShape->padfY[i] ); 
  }
  
  if (  Map->fInfo.shp.type == SHPT_POINT || Map->fInfo.shp.type == SHPT_POINTZ
        || Map->fInfo.shp.type == SHPT_MULTIPOINTM ) {
      Map->fInfo.shp.shape = shape + 1 ;
      Map->fInfo.shp.part = 0;
      
  } else {
      if ( part == pShape->nParts - 1 ) {  
	  Map->fInfo.shp.shape = shape + 1 ;
	  Map->fInfo.shp.part = 0;
      } else {
	  Map->fInfo.shp.shape = shape ;
	  Map->fInfo.shp.part = part + 1;
      }
  }

  SHPDestroyObject(pShape);
  
  switch ( Map->fInfo.shp.type ) {
      case SHPT_POINT :
      case SHPT_MULTIPOINT :
      case SHPT_POINTZ :
      case SHPT_MULTIPOINTZ :
      case SHPT_POINTM :
      case SHPT_MULTIPOINTM :
  	return (GV_POINT);
	
      case SHPT_ARC :
      case SHPT_ARCZ :
      case SHPT_ARCM :
  	return (GV_LINE);
	
      case SHPT_POLYGON :
      case SHPT_POLYGONZ :
      case SHPT_POLYGONM :
  	return (GV_BOUNDARY);
	
      default:
	G_warning ("Shape type %d not supported\n", Map->fInfo.shp.type);
  	return (GV_DEAD_POINT);
  }
}

