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
xtract_line (int num_index, int num_array[], struct Map_info *Map, struct Map_info *Out_Map,
             int cat_new, int select, int dissolve)
{
	int cat, cat1, areal, arear, catl, catr, centroid, line;
	int max_att=0, type;
	int i, n, tbtype, ret;
	struct line_pnts *Points, *CPoints;
	struct line_cats *Cats, *CCats;
	struct field_info *Fi, *Fin;

        /* Initialize the Point structure, ONCE */
        Points = Vect_new_line_struct();
        CPoints = Vect_new_line_struct();
	Cats = Vect_new_cats_struct ();
	CCats = Vect_new_cats_struct ();
	
	/* TODO: Dissolve common boundaries and output are centroids */ 
        /* Cycle through all lines */
        for ( line = 1; line <= Vect_get_num_lines ( Map ); line++) {
	     G_debug ( 2, "Line = %d", line );
	     type = Vect_read_line ( Map, Points, Cats, line);
	     
	     cat = catr = catl = 0;
	     /* get the line category */
	     Vect_cat_get ( Cats, 1, &cat );
	     
	     /* skip anything other than the selected line type and get the category */
	     if ( type == GV_BOUNDARY ) {
	         if ( !(select & GV_BOUNDARY) && !(select & GV_AREA) ) continue;
		 if ( select & GV_AREA ) { /* get left right category */
		     Vect_get_line_areas ( Map, line, &areal, &arear );
		     if ( areal > 0 ) {
			 centroid = Vect_get_area_centroid ( Map, areal );
			 if ( centroid > 0 ) {
	                     Vect_read_line ( Map, CPoints, CCats, centroid);
			     Vect_cat_get ( CCats, 1, &catr );
			 }
		     }
		     if ( arear > 0 ) {
			 centroid = Vect_get_area_centroid ( Map, arear );
			 if ( centroid > 0 ) {
	                     Vect_read_line ( Map, CPoints, CCats, centroid);
			     Vect_cat_get ( CCats, 1, &catr );
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
		     Vect_cat_set (Cats, 1, cat1);
		     Vect_write_line (Out_Map, type, Points, Cats);
		      
		     /* capture the highest attribute value */
		     if (cat1 > max_att) max_att = cat1;
		     break;
	         }
             } /* end for num_index */
        }  /* end lines section */

    /* Copy tables */
    fprintf (stdout,"Copying tables ...\n") ;
    n = Vect_get_num_dblinks ( Map );
    tbtype = GV_1TABLE;
    if ( n > 1 ) tbtype = GV_MTABLE;
    for ( i = 0; i < n; i++ ) {
	Fi = Vect_get_dblink ( Map, i );
	if ( Fi == NULL ) {
	    G_warning ( "Cannot get db link info -> cannot copy table." );
	    continue;
	}
	Fin = Vect_default_field_info ( Out_Map->name, Fi->number, Fi->name, tbtype );
        G_debug (3, "Copy drv:db:table '%s:%s:%s' to '%s:%s:%s'", 
	              Fi->driver, Fi->database, Fi->table, Fin->driver, Fin->database, Fin->table );
	Vect_map_add_dblink ( Out_Map, Fi->number, Fi->name, Fin->table, Fi->key, Fin->database, Fin->driver);
        
	ret = db_copy_table ( Fi->driver, Fi->database, Fi->table, 
		    Fin->driver, Vect_subst_var(Fin->database,Out_Map->name,G_mapset()), Fin->table );
	if ( ret == DB_FAILED ) {
	    G_warning ( "Cannot copy table" );
	    continue;
	}
    } /* for of copy table*/

	return(max_att) ;
}

