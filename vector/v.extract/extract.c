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
             int cat_new, int select_type, int dissolve, int field, int type_only)
{
	int left_cat_old, right_cat_old;
	int left_cat_new, right_cat_new;
	int areal, arear, centroid, line;
	int type;
	int i, j;
	struct line_pnts *Points, *CPoints;
	struct line_cats *Cats, *Line_Cats_Old, *Line_Cats_New, *CCats;

        /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();
        CPoints = Vect_new_line_struct();
	Line_Cats_Old = Vect_new_cats_struct ();
	Line_Cats_New = Vect_new_cats_struct ();
	CCats = Vect_new_cats_struct ();
	
	/* TODO: more categories of one field for dissolve boundaries */

        /* Cycle through all lines */
        for ( line = 1; line <= Vect_get_num_lines ( In ); line++) {
	     G_debug ( 2, "Line = %d", line );
	     type = Vect_read_line ( In, Points, Line_Cats_Old, line);

	     if ( type_only ) {
		 int write = 0;
		 
		 if ( type & select_type ) write = 1;

		 if ( type == GV_BOUNDARY && select_type & GV_AREA ) {
		     Vect_get_line_areas ( In, line, &areal, &arear );
                     if ( areal != 0 || arear != 0 ) {
			 if ( dissolve ) {
			     if ( areal < 0 || arear < 0 ) {
			 	write = 1;
			     }
			 } else {
			     write = 1;
			 }
		     }
		 }
		 
		 if ( type == GV_CENTROID  && select_type & GV_AREA ) {
		     areal = Vect_get_centroid_area ( In, line );
		     if ( areal > 1 ) write = 1;
		 }
	
                 if ( write ) Vect_write_line (Out, type, Points, Line_Cats_Old);
		 continue;
	     }
	     
	     left_cat_old = right_cat_old = -1;
	     
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

	    left_cat_new = right_cat_new = -1;
	    Vect_reset_cats ( Line_Cats_New );

	    /* check against the user category list */
	    for ( i = 0 ; i < num_index ; i++) {
		for ( j = 0; j < Line_Cats_Old->n_cats; j++ ) {
		    if ( Line_Cats_Old->field[j] == field && Line_Cats_Old->cat[j] == num_array[i] ) {
			if ( cat_new >= 0 )
			    Vect_cat_set ( Line_Cats_New, field, cat_new );
			else 
			    Vect_cat_set ( Line_Cats_New, field, Line_Cats_Old->cat[j] );
		    }
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
		}
	    }
		
	     /*  Note: type was previously checked, no need to recheck here again. */
	    if (  Line_Cats_New->n_cats > 0 
		 || 
		 ( type == GV_BOUNDARY && (select_type & GV_AREA) 
		   && ( left_cat_new >= 0 || right_cat_new >= 0) 
		   && ( !dissolve || (left_cat_new != right_cat_new) ) 
		 )
	       ) 
	    {
		/* Copy remaining cats */
		for ( j = 0; j < Line_Cats_Old->n_cats; j++ ) {
		    if ( Line_Cats_Old->field[j] != field ) {
			Vect_cat_set ( Line_Cats_New, Line_Cats_Old->field[j], Line_Cats_Old->cat[j] );
		    }
		}
		
		/* write line */
		Vect_write_line (Out, type, Points, Line_Cats_New);
		      
             } /* end for num_index */
        }  /* end lines section */

	return(0) ;
}

