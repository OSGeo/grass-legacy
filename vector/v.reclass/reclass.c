/*  From preliminary work (pull.vect) by Dave Gerdes - CERL
 *  created by:         R.L.Glenn, SCS
 *  modified to function by RB 5/2000
 *
 * Function reclass() reads input vector map and writes reclassed elements to output map.
 *
 * Arguments:
 * in_name - name of input vector map
 * out_name - name of output vector map
 * new - reclass table
 * type - elements type 
 * optiond - do not output boundaries between areas with the same cat 
 *
 * Returns:
 * number of elements created or -1 on error
 */

#include <stdlib.h>
#include <string.h>
#include  "gis.h"
#include  "dbmi.h"
#include "Vect.h"

int reclass ( struct Map_info *In, struct Map_info *Out, int type, int field, dbCatValArray *cvarr, int dissolve)
{
    int nlines, nareas, line, area, ltype, old_cat, new_cat;	
    int nocat = 0, rclelem=0;
    struct line_pnts *Points;
    struct line_cats *Cats;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();

    /* --------------------- Lines Section ------------------------------- */
    /* Cycle through all lines */
    nlines = Vect_get_num_lines (In);
    for ( line = 1; line <= nlines; line++) { 
	int written = 0;

	G_percent(line, nlines, 1);
	
	ltype = Vect_read_line ( In, Points, Cats, line );

	if ( (ltype & type) ) { /* GV_POINTS or GV_LINES */
	    Vect_cat_get ( Cats, field, &old_cat );

	    if ( old_cat < 0 ) continue;
	    
	    G_debug (3, "  old_cat = %d", old_cat ); 

	    if ( db_CatValArray_get_value_int ( cvarr, old_cat, &new_cat ) != DB_OK ) {
		nocat++;
	    } else { 
	        G_debug (3, "  new_cat = %d", new_cat ); 
		Vect_reset_cats ( Cats );
		Vect_cat_set ( Cats, 1, new_cat );

		Vect_write_line (Out, ltype, Points, Cats);
		rclelem++;
		written = 1;
	    }
	}

	if ( !written && (type & GV_AREA) && (ltype == GV_BOUNDARY) ) { /* Area boundary */
	    int left, right, lcat, rcat, new_lcat, new_rcat;
	    
	    /* Get the category for areas left & right */
	    lcat = rcat = 0;
	    Vect_get_line_areas ( In, line, &left, &right );

	    G_debug (3, "  left = %d right = %d", left, right ); 

	    if ( left < 0 ) left = Vect_get_isle_area ( In, abs(left) );
	    if ( left > 0 ) lcat = Vect_get_area_cat ( In, left, field );

	    if ( right < 0 ) right = Vect_get_isle_area ( In, abs(right) );
	    if ( right > 0 ) rcat = Vect_get_area_cat ( In, right, field );
	    
	    G_debug (3, "  lcat = %d rcat = %d", lcat, rcat ); 

	    if ( lcat < 0 || db_CatValArray_get_value_int ( cvarr, lcat, &new_lcat ) != DB_OK )
		new_lcat = 0;

	    if ( rcat < 0 || db_CatValArray_get_value_int ( cvarr, rcat, &new_rcat ) != DB_OK )
		new_rcat = 0;
	
	    /* if area not labeled */
	    if ( new_lcat == 0 && new_rcat == 0 ) continue;
	
	    /* if user requested -d option */
	    /* TODO: decide which centroid keep and store this info  */
	    /*
	    if (optiond && new_lcat == new_rcat ) {
		continue;
	    }
	    */

	    Vect_reset_cats ( Cats );
	    Vect_write_line (Out, ltype, Points, Cats);
	}
    }  /* end lines section */

    /* ------------------ Area attributes section ------------------- */
    if (type & GV_AREA) {
	nareas = Vect_get_num_areas (In);
	for (area = 1 ; area <= nareas ; area++) {
	    int centroid, old_cat, new_cat;
	    
	    /* TODO: Dissolve common boundaries */

	    G_debug (3, "area = %d", area ); 

	    centroid =  Vect_get_area_centroid ( In, area );
	    G_debug (3, "  centroid = %d", centroid ); 
	    if ( centroid <= 0 ) continue;
	
	    Vect_read_line ( In, Points, Cats, centroid );

	    Vect_cat_get ( Cats, field, &old_cat );
	    G_debug (3, "  old_cat = %d", old_cat ); 
	    if ( old_cat < 0 ) continue;

	    if ( db_CatValArray_get_value_int ( cvarr, old_cat, &new_cat ) != DB_OK ) {
		nocat++;
		continue;
	    }
	    G_debug (3, "  new_cat = %d", new_cat ); 

	    Vect_reset_cats ( Cats );
	    Vect_cat_set ( Cats, 1, new_cat );

	    Vect_write_line (Out, GV_CENTROID, Points, Cats);
	    rclelem++;
	} 
    }  /* end area attributes section */

    if ( nocat > 0 )
	G_warning ("For %d elements no new category was defined", nocat );

    return(rclelem) ;
}

