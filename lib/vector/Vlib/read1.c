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
   **  Read any level I line, given offset.
   **    This is NOT affected by constraints AND is identical to Vect__Read_line()
 */
int 
V1_read_line (
	       struct Map_info *Map,
	       struct line_pnts *Points,
	       struct line_cats *Cats,
	       long offset)
{
  return Vect__Read_line (Map, Points, Cats, offset);
}


/*
   **  returns -2 on EOF   -1 on Out of memory  or  line type 
 */
int 
V1_read_next_line (
		    struct Map_info *Map,
		    struct line_pnts *line_p,
		    struct line_cats *line_c)
{
  int itype;

  double n, s;
  double e, w;

  while (1)
    {
      itype = Vect__Read_line (Map, line_p, line_c, ftell (Map->dig_fp));
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
