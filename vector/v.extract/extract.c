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
	int cat, areal, arear, catl, catr, centroid, line;
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
	     
	     cat = catr = catl = -1;
	     /* get the line category */
	     Vect_cat_get ( Cats, field, &cat );
	     
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
			     Vect_cat_get ( CCats, field, &catl );
			 }
		     } 
		     
		     if ( arear < 0 ) 
			arear = Vect_get_isle_area ( In, abs(arear) ); 
		     if ( arear > 0 ) {
			 centroid = Vect_get_area_centroid ( In, arear );
			 if ( centroid > 0 ) {
	                     Vect_read_line ( In, CPoints, CCats, centroid);
			     Vect_cat_get ( CCats, field, &catr );
			 }
		     }
		 }
	     } else {
	         if ( !(type & select_type) )   continue;
	     }

	     /* check against the user category list */
             for ( i = 0 ; i < num_index ; i++) {
                 if ( cat == num_array[i] || catr == num_array[i] || catl == num_array[i] ) {  
		    if ( type == GV_BOUNDARY && dissolve ) {
			int have_left=FALSE, have_right=FALSE;

			for (j=0; j < num_index; j++) {
			    if(catl == num_array[j]) {
				have_left=TRUE;
				if(have_right) break;  /* we've got what we came for, no point looking any further */
			    }
			    if(catr == num_array[j]) {
				have_right=TRUE;
				if(have_left) break;
			    }
			}
			if(have_left && have_right)  continue;
		     }
		     /* write line */
		     if ( cat_new > 0 && cat >= 0 ) { /* assign the new category value */
			 Vect_field_cat_del ( Cats, field, -1 ); /* delete all cats of given field */
		         Vect_cat_set (Cats, field, cat_new); 
		     }
		     Vect_write_line (Out, type, Points, Cats);
		      
		     break;
	         }
             } /* end for num_index */
        }  /* end lines section */

	return(0) ;
}

