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

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

#ifdef HAVE_POSTGRES

int num_points(char *str);
int parse_points(char *str, int num_points, double *x,double *y,double *z);

/*
*  read line from database
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*
*  This is NOT affected by constraints
*/
int 
V1_read_line_post (
	       struct Map_info *Map,
	       struct line_pnts *Points,
	       struct line_cats *Cats,
	       long id)
{
  return Vect__Read_line_post (Map, Points, Cats, id);
}


/*
   **  returns -2 on EOF   -1 on Out of memory  or  line type 
 */
int 
V1_read_next_line_post (
		    struct Map_info *Map,
		    struct line_pnts *line_p,
		    struct line_cats *line_c)
{
  int itype;
  long id;

  double n, s;
  double e, w;
 
  id = ( Map->fInfo.post.nextRow );
  while (1)
    {
      itype = Vect__Read_line_post (Map, line_p, line_c, id);
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
	  /* if (itype & ELEMENT_TYPE_DEAD) */	/* is it DEAD? */
	  /*  continue; */
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
*  read line from database
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*/
int
Vect__Read_line_post (
		    struct Map_info *Map,
		    struct line_pnts *p,
		    struct line_cats *c,
		    long id)
{
  int i, np;
  int n_cats;
  int type;
  char  *coor;

  if ( id >= Map->fInfo.post.nGeom ) {
      return (-2); /* last record reached */ 
  }
  
  if ( c != NULL )
    {
      c->n_cats = 1;
      Vect_reset_cats ( c );
      /* TODO set cats to correct values */
      Vect_cat_set ( c, 1, id + 1 );
    }
 
  coor = PQgetvalue(Map->fInfo.post.geomRes, id, 0);  
  np = num_points ( coor );  
 
  if (0 > dig_alloc_points (p, np)) { return (-1); }
  p->n_points = np;
  
  parse_points( coor, np, p->x, p->y, p->z );
#ifdef GDEBUG
  for ( i = 0; i < np; i++ ) {
      G_debug (5, "i = %d : x,y = %f, %f", i, p->x[i], p->y[i]);
  }
#endif

  Map->fInfo.post.nextRow++;

  return (GV_LINE);  
}


/* Following routines are temporary solution (example) and
** must be replaced by binary cursors
*/

//returns how many points are in the first list in str
//
//  1. scan ahead looking for "("
//  2. find "," until hit a ")"
//  3. return number of points found
//	
// NOTE: doesnt actually parse the points, so if the 
//       str contains an invalid geometry, this could give
// 	   back the wrong answer.
//
// "(1 2 3, 4 5 6),(7 8, 9 10, 11 12 13)" => 2 (2nd list is not included)
int	num_points(char *str){
	int		keep_going;
	int		points_found = 1; //no "," if only one point (and last point)
						

	if ( (str == NULL) || (str[0] == 0) )
	{
		return 0;  //either null string or empty string
	}

	//look ahead for the "("

	str = strchr(str,'(') ;
	
	if ( (str == NULL) || (str[1] == 0) )  // str[0] = '(';
	{
		return 0;  //either didnt find "(" or its at the end of the string
	}

	keep_going = 1;
	while (keep_going) 
	{
		str=strpbrk(str,",)");  // look for a "," or ")"
		keep_going = (str != NULL);
		if (keep_going)  // found a , or )
		{
			if (str[0] == ')')
			{
				//finished
				return points_found;
			}
			else	//str[0] = ","
			{
				points_found++;
				str++; //move 1 char forward	
			}
		}
	}
	return points_found; // technically it should return an error.
}

//reads points into x,y,z co-ord arrays
int parse_points(char *str, int num_points, double *x,double *y,double *z){
	int	keep_going;
	int	num_found= 0;
	char	*end_of_double;
	
	if ( (str == NULL) || (str[0] == 0) ){
		return 0;  //either null string or empty string
	}
	
	//look ahead for the "("
	str = strchr(str,'(') ;
	
	if ( (str == NULL) || (str[1] == 0) ){  // str[0] = '(';
		return 0;  //either didnt find "(" or its at the end of the string
	}
	
	str++;  //move forward one char
	keep_going = 1;
	while (keep_going == 1){
		
		//attempt to get the point
		//scanf is slow, so we use strtod()

		x[num_found] = (double)strtod(str,&end_of_double);
		if (end_of_double == str){
			return 0; //error occured (nothing parsed)
		}
		str = end_of_double;
		y[num_found] = strtod(str,&end_of_double);
		if (end_of_double == str){
			return 0; //error occured (nothing parsed)
		}
		str = end_of_double;
		z[num_found] = strtod(str,&end_of_double); //will be zero if error occured
		str = end_of_double;
		num_found++;

		str=strpbrk(str,",)");  // look for a "," or ")"
		if (str != NULL && str[0] == ','){
			str++;
		}
		keep_going = (str != NULL) && (str[0] != ')');		
	}
	return num_found; 
}

#endif
