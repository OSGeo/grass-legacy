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
    G_debug (3, "V1_read_line_shp() offset = %d", offset);
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
	  //dig_bound_box2 (line_p, &n, &s, &e, &w, 16000L);	/*4.0 */

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
    int    node, shape, cat;
    long   offset;
    P_LINE *Line; 
    P_NODE *Node;
    
    G_debug (3, "V2_read_line_shp()");
    
    if (line < 1 || line > Map->plus.n_lines)	/* ALL DONE */
        return -2;

    Line = Map->plus.Line[line]; 
    if ( Line->type == GV_CENTROID ) {
        G_debug (3, "Centroid");
	node = Line->N1;
	Node = Map->plus.Node[node]; 
        
	/* coordinates */
	if ( line_p != NULL ) {
	    Vect_reset_line ( line_p );
	    Vect_append_point ( line_p, Node->x, Node->y );
	}
        
        /* category */
	if ( line_c != NULL ) {
	    Vect_reset_cats ( line_c );
	    offset = Line->offset;
	    shape = ( offset >> 11 ) & 0x1FFFFF ;
	    if ( Map->fInfo.shp.cat_col_num >= 0  ) {
		cat = DBFReadIntegerAttribute( Map->fInfo.shp.hDbf, shape, 
					       Map->fInfo.shp.cat_col_num );
		Vect_cat_set ( line_c, 1, cat );
	    }
	}
	
        return (GV_CENTROID);
    } else {
        return Vect__Read_line_shp (Map, line_p, line_c, Line->offset);
    }
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

  return V1_read_next_line_shp (Map, line_p, line_c);
/*  
  while (1)
    {
      line = Map->next_line;

      if (line > Map->plus.n_lines)
	return (-2);

      Line = &(Map->plus.Line[line]);

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
*/
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
  int n_cats;
  int type;
  int shape, part;
  int first, last; 
  SHPObject *pShape;
  int cat;
  
  G_debug (3, "Vect__Read_line_shp() offset = %d", offset);
  shape = ( offset >> 11 ) & 0x1FFFFF ;
  part = offset & 0x7FF;
  G_debug (3, "shape = %d part = %d", shape, part);
  
  G_debug (3, "nShapes = %d", Map->fInfo.shp.nShapes);
  if ( shape >= Map->fInfo.shp.nShapes ) {
      return (-2); /* EOF reached */ 
  }
  
  switch ( Map->fInfo.shp.type ) {
      case SHPT_POINT :
      case SHPT_MULTIPOINT :
      case SHPT_POINTZ :
      case SHPT_MULTIPOINTZ :
      case SHPT_POINTM :
      case SHPT_MULTIPOINTM :
  	type = GV_POINT;
	break;
	
      case SHPT_ARC :
      case SHPT_ARCZ :
      case SHPT_ARCM :
  	type = GV_LINE;
	break;
	
      case SHPT_POLYGON :
      case SHPT_POLYGONZ :
      case SHPT_POLYGONM :
  	type = GV_BOUNDARY;
	break;
	
      default:
	G_warning ("Shape type %d not supported\n", Map->fInfo.shp.type);
  	type = 0;
	break;
  }
  

  if ( c != NULL ) {
      Vect_reset_cats ( c );
      if ( Map->fInfo.shp.cat_col_num >= 0 &&
	   ( type == GV_POINT || type == GV_LINE ) ) {
	  cat = DBFReadIntegerAttribute( Map->fInfo.shp.hDbf, shape, 
		        Map->fInfo.shp.cat_col_num );
          Vect_cat_set ( c, 1, cat );
      }
  }
 
  
  if ( p != NULL ) {
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
  }
  
  return type;
}


/*
*  Returns  next line offset
*/
long
Vect_next_line_offset_shp ( struct Map_info *Map )
{
    long offset;
    
    offset = ( ( Map->fInfo.shp.shape << 11 ) | ( Map->fInfo.shp.part & 0x7FF) );
    
    return offset;
}
