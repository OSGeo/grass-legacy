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
             int cat_new, int select, int dissolve, int field_in, int field_out)
{
	int cat, cat1, areal, arear, catl, catr, centroid, line;
	int max_att=0, type;
	int i;
	struct line_pnts *Points, *CPoints;
	struct line_cats *Cats, *CCats;

        /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();
        CPoints = Vect_new_line_struct();
	Cats = Vect_new_cats_struct ();
	CCats = Vect_new_cats_struct ();
	
	/* TODO: Dissolve common boundaries and output are centroids */ 
        /* Cycle through all lines */
        for ( line = 1; line <= Vect_get_num_lines ( In ); line++) {
	     G_debug ( 2, "Line = %d", line );
	     type = Vect_read_line ( In, Points, Cats, line);
	     
	     cat = catr = catl = 0;
	     /* get the line category */
	     Vect_cat_get ( Cats, field_in, &cat );
	     
	     /* skip anything other than the selected line type and get the category */
	     if ( type == GV_BOUNDARY ) {
	         if ( !(select & GV_BOUNDARY) && !(select & GV_AREA) ) continue;
		 if ( select & GV_AREA ) { /* get left right category */
		     Vect_get_line_areas ( In, line, &areal, &arear );
		     if ( areal > 0 ) {
			 centroid = Vect_get_area_centroid ( In, areal );
			 if ( centroid > 0 ) {
	                     Vect_read_line ( In, CPoints, CCats, centroid);
			     Vect_cat_get ( CCats, field_in, &catl );
			 }
		     }
		     if ( arear > 0 ) {
			 centroid = Vect_get_area_centroid ( In, arear );
			 if ( centroid > 0 ) {
	                     Vect_read_line ( In, CPoints, CCats, centroid);
			     Vect_cat_get ( CCats, field_in, &catr );
			 }
		     }
		 }
	     } else {
	         if ( !(type & select) )   continue;
	     }

	     /* check against the user category list */
             for ( i = 0 ; i < num_index ; i++) {
                 if ( cat == num_array[i] || catr == num_array[i] || catl == num_array[i] ) {  
	             if ( type == GV_BOUNDARY && dissolve ) {
			 /* TODO */
		     }
		     /* write line */
		     cat1 = cat_new ? cat_new : num_array[i];
		     Vect_cat_set (Cats, field_out, cat1);
		     Vect_write_line (Out, type, Points, Cats);
		      
		     /* capture the highest attribute value */
		     if (cat1 > max_att) max_att = cat1;
		     break;
	         }
             } /* end for num_index */
        }  /* end lines section */

	return(max_att) ;
}

