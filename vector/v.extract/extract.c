/*  @(#)xtract_lines.c    1.0  9/29/89   
 *  created by:         R.L.Glenn, SCS
 *
 * Program will read vector line records, outputting lines
 * which match the user list of names/categories.
 * The resulting map attribute is arbitarily set to first category
 * of the user list or a user selected category number (cat_new).
 */

#include <string.h>
#include <stdlib.h>
#include  "gis.h"
#include "Vect.h"
#include "dbmi.h"

int 
xtract_line (int num_index, int num_array[], struct Map_info *In, struct Map_info *Out,
             int cat_new, int select_type, int dissolve, int field)
{
	int line_cat_old, left_cat_old, right_cat_old;
	int line_cat_new, left_cat_new, right_cat_new;
	int areal, arear, centroid, line;
	int type;
	int i, j;
	struct line_pnts *Points, *CPoints;
	struct line_cats *Cats, *CCats;

        /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();
        CPoints = Vect_new_line_struct();
	Cats = Vect_new_cats_struct ();
	CCats = Vect_new_cats_struct ();
	
	/* TODO: more categories of one field */

        /* Cycle through all lines */
        for ( line = 1; line <= Vect_get_num_lines ( In ); line++) {
	     G_debug ( 2, "Line = %d", line );
	     type = Vect_read_line ( In, Points, Cats, line);
	     
	     line_cat_old = left_cat_old = right_cat_old = -1;
	     /* get the line category */
	     Vect_cat_get ( Cats, field, &line_cat_old );
	     
	    /* skip anything other than the selected line type and get the category */
	    if ( type == GV_BOUNDARY ) {
	        if ( !(select_type & GV_BOUNDARY) && !(select_type & GV_AREA) ) continue;
		if ( select_type & GV_AREA ) { /* get left right category */
		     Vect_get_line_areas ( In, line, &areal, &arear );

		     if ( areal < 0 ) 
			areal = Vect_get_isle_area ( In, abs(areal) ); 
		     if ( areal > 0 ) {
			 centroid = Vect_get_area_centroid ( In, areal );
			 if ( centroid > 0 ) {
	                     Vect_read_line ( In, CPoints, CCats, centroid);
			     Vect_cat_get ( CCats, field, &left_cat_old );
			 }
		     } 
		     
		     if ( arear < 0 ) 
			arear = Vect_get_isle_area ( In, abs(arear) ); 
		     if ( arear > 0 ) {
			 centroid = Vect_get_area_centroid ( In, arear );
			 if ( centroid > 0 ) {
	                     Vect_read_line ( In, CPoints, CCats, centroid);
			     Vect_cat_get ( CCats, field, &right_cat_old );
			 }
		     }
		}
	    } else {
	        if ( !(type & select_type) )   continue;
	    }

	    line_cat_new = left_cat_new = right_cat_new = -1;

	    /* check against the user category list */
	    for ( i = 0 ; i < num_index ; i++) {
                if ( line_cat_old == num_array[i] ) {
		    if ( cat_new >= 0 )
		        line_cat_new = cat_new;
		    else 
			line_cat_new = line_cat_old;
		}


		if ( type == GV_BOUNDARY && (select_type & GV_AREA) ) {
		    if ( left_cat_old == num_array[i] ) {
			if ( cat_new >= 0 )
			    left_cat_new = cat_new;
			else 
			    left_cat_new = left_cat_old;
		    }
		    
		    if ( right_cat_old == num_array[i] ) {
			if ( cat_new >= 0 )
			    right_cat_new = cat_new;
			else 
			    right_cat_new = right_cat_old;
		    }

		    if ( line_cat_old >= 0 && left_cat_new >= 0 && right_cat_new >= 0 ) {
			/* all found */
			break;
		    }
		} else { 
		    if ( line_cat_new >= 0 ) {
			/* found */
			break;
		    }
		}
	    }

		
	     /*  Note: type was previously checked, no need to recheck here again. */
	    if ( line_cat_new >= 0 
		 || 
		 ( type == GV_BOUNDARY && (select_type & GV_AREA) 
		   && ( left_cat_new >= 0 || right_cat_new >= 0) 
		   && ( !dissolve || (left_cat_new != right_cat_new) ) 
		 )
	       ) 
	    {
		Vect_field_cat_del ( Cats, field, -1 ); /* delete all cats of given field */

		/* Set new category */
		if ( line_cat_new >= 0 )
		     Vect_cat_set (Cats, field, line_cat_new); 
		
		/* write line */
		Vect_write_line (Out, type, Points, Cats);
		      
             } /* end for num_index */
        }  /* end lines section */

	return(0) ;
}

