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

int 
write_line ( struct Map_info *Map, int type, struct line_pnts *Points )
{
    int i, cat, ret;
    static int first_form = 1;
    char buf[2000], *form;
    struct line_cats *Cats;
    struct field_info *Fi;
    dbDriver *driver;
    dbValue value;
    dbString sql, html;
    
    Cats = Vect_new_cats_struct ();
    db_init_string (&sql);
    db_init_string (&html);
    
    cat = var_geti(VAR_CAT);
    if ( var_geti(VAR_CAT_MODE) != CAT_MODE_NO &&  cat > 0 && var_geti(VAR_FIELD) > 0 ) {
        Vect_cat_set ( Cats, var_geti(VAR_FIELD), cat ); 
	
        G_debug (2, "write field = %d cat = %d", var_geti(VAR_FIELD), var_geti(VAR_CAT)); 

        if ( cat_max_get( var_geti(VAR_FIELD) ) < var_geti(VAR_CAT)  ) {
	    cat_max_set( var_geti(VAR_FIELD), var_geti(VAR_CAT) );
        }
    }
    
    ret = Vect_write_line ( Map, type, Points, Cats );

    for ( i = 0; i < Vect_get_num_updated_lines(Map); i++ )
	G_debug (2, "Updated line: %d", Vect_get_updated_line( Map, i ) );

    for ( i = 0; i < Vect_get_num_updated_nodes(Map); i++ )
	G_debug (2, "Updated node: %d", Vect_get_updated_node( Map, i ) );

    /* Reset category (this automaticaly resets cat for next not used) */
    var_seti ( VAR_FIELD, var_geti ( VAR_FIELD ) );

    if ( var_geti(VAR_CAT_MODE) != CAT_MODE_NO && var_geti(VAR_INSERT) && cat > 0 ) {
	G_debug (2, "Insert new record" );
        db_set_string (&html, "<HTML><HEAD><TITLE>Form</TITLE><BODY>");
	Fi = Vect_get_field( Map, var_geti(VAR_FIELD) );
	if ( Fi == NULL ) { 
	    i_message ( MSG_OK, MSGI_ERROR, "Database table for this field is not defined" );
            return -1;
	}

	/* Note: some drivers (dbf) writes date when db is closed so it is better open
	 * and close database for each record, so that data may not be lost later */

	/* First check if already exists */
        driver = db_start_driver_open_database ( Fi->driver, Fi->database );
	if ( driver == NULL ) {
	    sprintf (buf, "Cannot open database %s by driver %s", Fi->database, Fi->driver );
	    i_message ( MSG_OK, MSGI_ERROR, buf );
            return -1;
	}
        ret = db_select_value ( driver, Fi->table, Fi->key, cat, Fi->key, &value );
	if ( ret == -1 ) {
            db_close_database_shutdown_driver ( driver );
	    sprintf (buf, "Cannot select record from table %s", Fi->table );
	    i_message ( MSG_OK, MSGI_ERROR, buf );
            return -1;
	}
	if ( ret == 0 ) { /* insert new record */
	    sprintf ( buf, "insert into %s (%s) values (%d)", Fi->table, Fi->key, cat );
	    db_set_string ( &sql, buf);
	    G_debug ( 2, db_get_string ( &sql ) );
	    ret = db_execute_immediate (driver, &sql);
	    if ( ret != DB_OK ) {	
                db_close_database_shutdown_driver ( driver );
	        sprintf (buf, "Cannot insert new record: %s", db_get_string(&sql) );
	        i_message ( MSG_OK, MSGI_ERROR, buf );
                return -1;
	    }
	    db_append_string (&html, "New record was created.<BR>");
        } else { /* record already existed */
	    db_append_string (&html, "Record for this category already existed.<BR>");
	}
	
        db_close_database_shutdown_driver ( driver );

	/* Open form */
        F_generate ( Fi->driver, Fi->database, Fi->table, Fi->key, cat, NULL, NULL, 
		                  F_EDIT, F_HTML, &form);
	db_append_string (&html, form );
	db_append_string (&html, "</BODY></HTML>");
    
	/* Note: F_open() must be run first time with closed monitor, otherwise next
	*        attempt to open driver hangs until form child process is killed */
	if ( first_form ) driver_close();
	F_clear ();
	F_open ( "Attributes", db_get_string(&html) );
	if ( first_form ) { driver_open(); first_form = 0; }

        G_free (form);	
	db_free_string (&html);

    }

    return 0;
}

/* Snap to node */
int snap ( double *x, double *y ) 
{
    int node;
    double thresh;
    
    G_debug (2, "snap(): x = %f, y = %f", *x, *y); 
    
    if ( !var_geti(VAR_SNAP) ) return 0;
    
    if ( var_geti(VAR_SNAP_MODE) == SNAP_MAP ) {
	thresh = var_getd(VAR_SNAP_MAP);
    } else {
	thresh = Scale * var_geti(VAR_SNAP_SCREEN);
    }
    
    node = Vect_find_node ( &Map, *x, *y, 0, thresh, 0 );

    if ( node > 0 ) Vect_get_node_coor (&Map, node, x, y, NULL );
	
    G_debug (2, "node = %d x = %f, y = %f", node, *x, *y); 
    return node;
}

/* Digitize new line */
int new_line ( int type )
{
    int i, sxo = 0, syo = 0, sxn, syn;
    int button, first, line, node1, node2;
    double x, y;
    char buf[1000];
    struct line_pnts *Points;
    struct line_cats *Cats;
    
    G_debug (2, "new_line(): type = %d", type);

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    sprintf ( buf, "Digitize new %s:", get_line_type_name (type) );
    i_prompt ( buf ); 
    i_prompt_buttons ( "New point", "New point", "Quit tool"); 
    
    i_new_line_options ( 1 );
    
    driver_open();
    
    first = 1; 
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
	if ( first ) {
	    R_get_location_with_pointer ( &sxn, &syn, &button); 
            i_prompt_buttons ( "New point", "Undo last point", "Close line"); 
        } else R_get_location_with_line (sxo, syo, &sxn, &syn, &button); 
	
	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	if ( button == 0 ) break; /* Tool broken by GUI */

	if ( first &&  button == 3 ) { /* Quit tool ( points & lines ), first is always for points */
	    Tool_next = TOOL_NOTHING; 
	    break;
	}

	if ( type & GV_POINTS ) {
	    /* We can get here with button = 1 or 2 -> the same write point */
	    snap ( &x, &y );
            Vect_append_point ( Points, x, y, 0 );	

	    write_line ( &Map, type, Points );
	    updated_lines_and_nodes_erase_refresh_display();
	    break;
	} else { /* GV_LINES */
	    /* Button may be 1,2,3 */
	    if ( button == 1 ) { /* New point */
		snap ( &x, &y );
                Vect_append_point ( Points, x, y, 0 );	
		
		if ( type == GV_LINE ) symb_set_driver_color ( SYMB_LINE );
		else symb_set_driver_color ( SYMB_BOUNDARY_0 );

		display_points ( Points, 1);
		sxo = D_u_to_d_col(x); syo = D_u_to_d_row (y);;
		first = 0;
	    } else if ( button == 2 ) { /* Undo last point */
		if ( Points->n_points >= 1 ) {
		    symb_set_driver_color ( SYMB_BACKGROUND ); 
		    display_points ( Points, 1);
		    Points->n_points--;

		    if ( type == GV_LINE ) symb_set_driver_color ( SYMB_LINE );
		    else symb_set_driver_color ( SYMB_BOUNDARY_0 );

                    display_points ( Points, 1);
		    sxo = D_u_to_d_col ( Points->x[Points->n_points - 1] );
		    syo = D_u_to_d_row ( Points->y[Points->n_points - 1] );
		}
		if ( Points->n_points == 0 ) {
                    i_prompt_buttons ( "New point", "New point", "Quit tool"); 
		    first = 1;
		}
	    } else { /* button = 3 -> write the line and quit */
		if ( Points->n_points > 1 ) {
		    /* Before the line is written, we must check if connected to existing nodes, if yes,
		     * such nodes must be add to update list before! the line is written (areas/isles */
                    node1 = Vect_find_node(&Map, Points->x[0], Points->y[0], Points->z[0], 0, Vect_is_3d(&Map)); 
		    i = Points->n_points - 1;
                    node2 = Vect_find_node(&Map, Points->x[i], Points->y[i], Points->z[i], 0, Vect_is_3d(&Map)); 
		    
	            G_debug (2, "  old node1 = %d  old node2 = %d", node1, node2);
	            line = write_line ( &Map, type, Points );
		    updated_lines_and_nodes_erase_refresh_display();
		} else G_warning ("Less than 2 points for line -> nothing written" );
		    
		break;
	    }
	    G_debug (2, "n_points = %d", Points->n_points);
	}
    }
    driver_close();
   
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    i_new_line_options ( 0 );
    
    G_debug (3, "new_line(): End");

    return 1;
}

/* Delete line */
int delete_line (void)
{
    int i, sxn, syn, line, last_line, node1, node2;
    int button, first;
    double x, y, thresh;
    struct line_pnts *Points;
    struct line_cats *Cats;
    
    G_debug (2, "delete_line()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    i_prompt ( "Delete line:"); 
    i_prompt_buttons ( "Select", "Unselect", "Quit tool"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    line = 0;
    first = 1;
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
            /* Delete last if any */
	    if ( last_line > 0 ) { 
		/* Erase line and nodes !!! (because if the line is not connected to any other, nodes will die */
		display_line ( last_line, SYMB_BACKGROUND, 1 );
		Vect_get_line_nodes ( &Map, line, &node1, &node2 ); 
		G_debug (2, "delete line = %d node1 = %d node2 = %d", last_line, node1, node2);

                display_node ( node1, SYMB_BACKGROUND, 1);
                display_node ( node2, SYMB_BACKGROUND, 1);
		
		Vect_delete_line ( &Map, last_line ); 

		for ( i = 0; i < Vect_get_num_updated_lines(&Map); i++ )
		    G_debug (2, "Updated line: %d", Vect_get_updated_line( &Map, i ) );

		for ( i = 0; i < Vect_get_num_updated_nodes(&Map); i++ )
		    G_debug (2, "Updated node: %d", Vect_get_updated_node( &Map, i ) );

		updated_lines_and_nodes_erase_refresh_display();
	    }
	    
	    /* Find neares point or line */
	    line = Vect_find_line (&Map, x, y, 0, GV_POINT|GV_CENTROID, thresh, 0, 0);
	    G_debug (2, "point found = %d", line );
	    if ( line == 0 ) line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
	    G_debug (2, "line found = %d", line );
	    
	    /* Display new selected line if any */
	    if ( line > 0 ) {
		display_line ( line, SYMB_HIGHLIGHT, 1);
	    }
	} else { /* button == 2 -> unselect */
	    line = 0;
	}
	
	if ( line > 0 ) 
	    i_prompt_buttons ( "Confirm and select next", "Unselect", "Quit tool"); 
	else
	    i_prompt_buttons ( "Select", "Unselect", "Quit tool"); 
	
	last_line = line;
	first = 0;
    }

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "delete_line(): End");

    return 1;
}

/* Move line */
int move_line (void)
{
    int i, sxn, syn, sxo = 0, syo = 0, line, last_line, node1, node2, type;
    int button, first;
    double x, y, thresh, xo, yo;
    struct line_pnts *Points;
    struct line_cats *Cats;
    
    G_debug (2, "move_line()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    i_prompt ( "Move line:"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    first = 1; 
    last_line = 0;
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
	if ( last_line == 0 ) {
            i_prompt_buttons ( "Select", "", "Quit tool"); 
	    R_get_location_with_pointer ( &sxn, &syn, &button); 
        } else R_get_location_with_line (sxo, syo, &sxn, &syn, &button); 
	
	if ( last_line > 0 ) {
	    display_line ( last_line, SYMB_DEFAULT, 1);
	}

	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	if ( button == 0 || button == 3 ) break;

	if ( button == 1 ) { /* Select / new location */
	    if ( last_line == 0 ) { /* Select line */ 
	        line = Vect_find_line (&Map, x, y, 0, GV_POINT|GV_CENTROID, thresh, 0, 0);
		G_debug (2, "point found = %d", line );
                if ( line == 0 ) line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
                G_debug (2, "line found = %d", line );
                
                /* Display new selected line if any */
                if ( line > 0 ) {
		    display_line ( line, SYMB_HIGHLIGHT, 1);

		    /* Find the nearest point on the line */
		    type = Vect_read_line ( &Map, Points, NULL, line );
		    Vect_line_distance ( Points, x, y, 0, 0, &xo, &yo, NULL, NULL, NULL, NULL );
                    sxo = D_u_to_d_col ( xo ) ; 
		    syo = D_u_to_d_row ( yo );


                    i_prompt_buttons ( "New location", "Unselect", "Quit tool"); 
	        }
		last_line = line;
	    } else { /* Line is already selected */
	        display_line ( last_line, SYMB_BACKGROUND, 1);
		Vect_get_line_nodes ( &Map, last_line, &node1, &node2 ); 
                display_node ( node1, SYMB_BACKGROUND, 1);
                display_node ( node2, SYMB_BACKGROUND, 1);

		type = Vect_read_line ( &Map, Points, Cats, last_line );
		for ( i = 0; i < Points->n_points; i++ ) {
                    Points->x[i] = Points->x[i] + x - xo;
                    Points->y[i] = Points->y[i] + y - yo;
		}
		Vect_rewrite_line(&Map, last_line, type, Points, Cats);
		updated_lines_and_nodes_erase_refresh_display();
		last_line = 0;
	    }

	}
	if ( button == 2 ) { /* Unselect */
	    if ( last_line > 0 ) {
		last_line = 0;
	    }
	}
    }

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "move_line(): End");

    return 1;
}


/* Move vertex */
int move_vertex (void)
{
    int sxn, syn, sxo = 0, syo = 0, line, last_line, last_vert, node1, node2, type, seg;
    int button, first;
    double x, y, thresh, xo, yo, dist;
    struct line_pnts *Points;
    struct line_cats *Cats;
    
    G_debug (2, "move_vertex()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    i_prompt ( "Move vertex:"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    first = 1; 
    last_line = 0;
    last_vert = 0;
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
	if ( last_line == 0 ) {
            i_prompt_buttons ( "Select", "", "Quit tool"); 
	    R_get_location_with_pointer ( &sxn, &syn, &button); 
        } else R_get_location_with_line (sxo, syo, &sxn, &syn, &button); 
	
	if ( last_line > 0 ) {
	    display_line ( last_line, SYMB_DEFAULT, 1);
	}

	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	if ( button == 0 || button == 3 ) break;

	if ( button == 1 ) { /* Select / new location */
	    if ( last_line == 0 ) { /* Select line */ 
                line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
                G_debug (2, "line found = %d", line );
                
                /* Display new selected line if any */
                if ( line > 0 ) {
		    display_line ( line, SYMB_HIGHLIGHT, 1);

		    /* Find the nearest vertex on the line */
		    type = Vect_read_line ( &Map, Points, NULL, line );
		    seg = Vect_line_distance ( Points, x, y, 0, 0, &xo, &yo, NULL, NULL, NULL, NULL );

		    dist = Vect_points_distance ( xo, yo, 0, Points->x[seg-1], Points->y[seg-1], 0, 0);

		    if ( dist < Vect_points_distance ( xo, yo, 0, Points->x[seg], Points->y[seg], 0, 0) ) {
			seg -= 1;
		    }
		    
		    xo = Points->x[seg];
		    yo = Points->y[seg];
                    sxo = D_u_to_d_col ( xo  ) ; 
		    syo = D_u_to_d_row ( yo );


                    i_prompt_buttons ( "New location", "Unselect", "Quit tool"); 
		    last_line = line;
		    last_vert = seg;
	        }
	    } else { /* Line is already selected */
	        display_line ( last_line, SYMB_BACKGROUND, 1);
		Vect_get_line_nodes ( &Map, last_line, &node1, &node2 ); 
                display_node ( node1, SYMB_BACKGROUND, 1);
                display_node ( node2, SYMB_BACKGROUND, 1);

		type = Vect_read_line ( &Map, Points, Cats, last_line );
                Points->x[last_vert] = Points->x[last_vert] + x - xo;
                Points->y[last_vert] = Points->y[last_vert] + y - yo;
		Vect_rewrite_line(&Map, last_line, type, Points, Cats);
		updated_lines_and_nodes_erase_refresh_display();
		last_line = 0;
	    }

	}
	if ( button == 2 ) { /* Unselect */
	    if ( last_line > 0 ) {
		last_line = 0;
	    }
	}
    }

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "move_line(): End");

    return 1;
}
