#include <stdio.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "form.h"
#include "global.h"
#include "proto.h"

/* Display attributes */
int display_attributes (void)
{
    int j, sxn, syn, line, last_line, type;
    int button;
    static int first_form = 1;
    double x, y, thresh;
    struct line_pnts *Points;
    struct line_cats *Cats;
    char buf[1000], title[500];
    char *form;
    dbString html; 
    struct field_info *Fi;
    
    G_debug (2, "display_attributes()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    db_init_string (&html);
    
    i_prompt ( "Display attributes:"); 
    i_prompt_buttons ( "Select line", "", "Quit tool"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    F_clear ();
    line = 0;
    last_line = 0;
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
        R_get_location_with_pointer ( &sxn, &syn, &button); 
	    
	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	/* Display last highlighted in normal color */
	if ( last_line > 0 ) {
	    display_line ( last_line, SYMB_DEFAULT, 1);
	}

	if ( button == 0 || button == 3 ) break; /* Quit tool */

	if ( button == 1 ) { /* Confirm / select */
	    F_clear ();
	    /* Find nearest point or line (points first!) */
	    line = Vect_find_line (&Map, x, y, 0, GV_POINTS, thresh, 0, 0);
	    G_debug (2, "point found = %d", line );
	    if ( line == 0 ) line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
	    G_debug (2, "line found = %d", line );
	    
	    /* Display new selected line if any */
	    if ( line > 0 ) {
		display_line ( line, SYMB_HIGHLIGHT, 1);
		type = Vect_read_line(&Map, Points, Cats, line);

		/* Note: F_open() must be run first time with closed monitor, otherwise next
		 *         *        attempt to open driver hangs until form child process is killed */
		if ( first_form ) { 
		     driver_close();
		     F_open ( "", "" );
	             F_clear ();
		     driver_open(); 
		     first_form = 0; 
		}

		if ( Cats->n_cats > 0 ) {
		    for (j = 0; j < Cats->n_cats; j++) {
			G_debug(3, "field = %d category = %d", Cats->field[j], Cats->cat[j]);

			sprintf (title, "Field %d", Cats->field[j] );
			db_set_string (&html, ""); 
			db_append_string (&html, "<HTML><HEAD><TITLE>Attributes</TITLE><BODY>"); 

			sprintf(buf, "field: %d<BR>category: %d<BR>", Cats->field[j], Cats->cat[j] );
			db_append_string (&html, buf);

			Fi = Vect_get_field( &Map, Cats->field[j]);
			if (Fi == NULL) {
			    db_append_string (&html, "Database connection not defined<BR>" );
			} else {
			    sprintf(buf, "driver: %s<BR>database: %s<BR>table: %s<BR>key column: %s<BR>",
					 Fi->driver, Fi->database, Fi->table, Fi->key);
			    db_append_string (&html, buf);
			    
			    F_generate ( Fi->driver, Fi->database, Fi->table, Fi->key, Cats->cat[j], 
				     NULL, NULL, F_EDIT, F_HTML, &form);
			    
			    db_append_string (&html, form); 
			    G_free (form);
			    G_free(Fi);
			}
			db_append_string (&html, "</BODY></HTML>"); 
			G_debug ( 3, db_get_string (&html) ); 
			F_open ( title, db_get_string(&html) );
		    }
		} else {
		    sprintf (title, "Line %d", line );
		    db_set_string (&html, ""); 
		    db_append_string (&html, "<HTML><HEAD><TITLE>Attributes</TITLE><BODY>"); 
		    db_append_string (&html, "No categories"); 
		    db_append_string (&html, "</BODY></HTML>"); 
		    G_debug ( 3, db_get_string (&html) ); 
		    F_open ( title, db_get_string(&html) );
		}
	    }
	    last_line = line;
	}
    }
    F_clear ();
    F_close ();

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "display_attributes(): End");

    return 1;
}
