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

/* Split line */
int split_line (void)
{
    int sxn, syn, line, last_line, last_seg, node1, node2, type, seg, i, np;
    int button, first;
    double x, y, thresh, xo, yo;
    struct line_pnts *Points, *NPoints;
    struct line_cats *Cats;
    
    G_debug (2, "split_line()");

    Points = Vect_new_line_struct ();
    NPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    i_prompt ( "Split line:"); 
    i_prompt_buttons ( "Select", "", "Quit tool"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    first = 1; 
    last_line = 0;
    last_seg = 0;
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
	R_get_location_with_pointer ( &sxn, &syn, &button); 
	if ( last_line == 0 ) {
            i_prompt_buttons ( "Select", "", "Quit tool"); 
        } 
	
	if ( last_line > 0 ) {
	    display_line ( last_line, SYMB_DEFAULT, 1);
	}

	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	if ( button == 0 || button == 3 ) break;

	if ( button == 1 ) { /* Select / split */
	    if ( last_line > 0 ) { /* Line is already selected -> split */
	        display_line ( last_line, SYMB_BACKGROUND, 1);
		Vect_get_line_nodes ( &Map, last_line, &node1, &node2 ); 
                display_node ( node1, SYMB_BACKGROUND, 1);
                display_node ( node2, SYMB_BACKGROUND, 1);
		symb_set_driver_color ( SYMB_BACKGROUND );
		display_icon ( xo, yo, G_ICON_CROSS, 0, 10, 1);

		/* Read and delete old */
		type = Vect_read_line ( &Map, Points, Cats, last_line );
		Vect_delete_line ( &Map, last_line );
		updated_lines_and_nodes_erase_refresh_display();
		np = Points->n_points;

		/* First part */
		Vect_reset_line ( NPoints );
		for ( i = 0; i < last_seg; i++ ){ 
		    Vect_append_point (NPoints, Points->x[i], Points->y[i], Points->z[i] );
		}
		Vect_append_point (NPoints, xo, yo, 0 );
		Vect_write_line( &Map, type, NPoints, Cats);
		updated_lines_and_nodes_erase_refresh_display();

		/* Second part */
		Vect_reset_line ( NPoints );
		Vect_append_point (NPoints, xo, yo, 0 );
		for ( i = last_seg; i < np; i++ ){ 
		    Vect_append_point (NPoints, Points->x[i], Points->y[i], Points->z[i] );
		}
		Vect_write_line( &Map, type, NPoints, Cats);
		updated_lines_and_nodes_erase_refresh_display();

		last_line = 0;
	    } 

	    /* Select vertex */ 
	    line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
	    G_debug (2, "line found = %d", line );
	    
	    /* Display new selected line if any */
	    if ( line > 0 ) {

		/* Find the nearest vertex on the line */
		type = Vect_read_line ( &Map, Points, NULL, line );
		seg = Vect_line_distance ( Points, x, y, 0, 0, &xo, &yo, NULL, NULL, NULL, NULL );

	        display_line ( line, SYMB_HIGHLIGHT, 1);
		symb_set_driver_color ( SYMB_HIGHLIGHT );
		display_icon ( xo, yo, G_ICON_CROSS, 0, 10, 1);

		i_prompt_buttons ( "Confirm and select next", "Unselect", "Quit tool"); 
		last_line = line;
		last_seg = seg;
	    }
	}
	if ( button == 2 ) { /* Unselect */
	    if ( last_line > 0 ) {
		symb_set_driver_color ( SYMB_BACKGROUND );
		display_icon ( xo, yo, G_ICON_CROSS, 0, 10, 1);
		last_line = 0;
	    }
	}
    }
    if ( last_line > 0 ) {
	symb_set_driver_color ( SYMB_BACKGROUND );
	display_icon ( xo, yo, G_ICON_CROSS, 0, 10, 1);
    }

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "split_line(): End");

    return 1;
}

/* Remove line vertex */
int rm_vertex (void)
{
    int sxn, syn, line, last_line, last_seg, node1, node2, type, seg, i, np;
    int button, first;
    double x, y, thresh, xo, yo, dist;
    struct line_pnts *Points;
    struct line_cats *Cats;
    
    G_debug (2, "remove_vertex()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    i_prompt ( "Remove vertex:"); 
    i_prompt_buttons ( "Select vertex", "", "Quit tool"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    first = 1; 
    last_line = 0;
    last_seg = 0;
    sxn = COOR_NULL; syn = COOR_NULL;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
	R_get_location_with_pointer ( &sxn, &syn, &button); 
	if ( last_line == 0 ) {
            i_prompt_buttons ( "Select vertex", "", "Quit tool"); 
        } 
	
	if ( last_line > 0 ) {
	    display_line ( last_line, SYMB_DEFAULT, 1);
	}

	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);

	if ( button == 0 || button == 3 ) break;

	if ( button == 1 ) { /* Select / new location */
	    if ( last_line > 0 ) { /* Line is already selected */
	        display_line ( last_line, SYMB_BACKGROUND, 1);
		Vect_get_line_nodes ( &Map, last_line, &node1, &node2 ); 
                display_node ( node1, SYMB_BACKGROUND, 1);
                display_node ( node2, SYMB_BACKGROUND, 1);
		symb_set_driver_color ( SYMB_BACKGROUND );
		display_icon ( xo, yo, G_ICON_BOX, 0, 10, 1);

		type = Vect_read_line ( &Map, Points, Cats, last_line );
		np = Points->n_points;
		for ( i = last_seg; i < np - 1; i++ ){ 
                    Points->x[i] = Points->x[i+1];
                    Points->y[i] = Points->y[i+1];
                    Points->z[i] = Points->z[i+1];
		}
		Points->n_points--;
		Vect_rewrite_line(&Map, last_line, type, Points, Cats);
		updated_lines_and_nodes_erase_refresh_display();
		last_line = 0;
	    } 

	    /* Select vertex */ 
	    line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
	    G_debug (2, "line found = %d", line );
	    
	    /* Display new selected line if any */
	    if ( line > 0 ) {

		/* Find the nearest vertex on the line */
		type = Vect_read_line ( &Map, Points, NULL, line );
		seg = Vect_line_distance ( Points, x, y, 0, 0, &xo, &yo, NULL, NULL, NULL, NULL );

		dist = Vect_points_distance ( xo, yo, 0, Points->x[seg-1], Points->y[seg-1], 0, 0);

		if ( dist < Vect_points_distance ( xo, yo, 0, Points->x[seg], Points->y[seg], 0, 0) ) {
		    seg -= 1;
		}
		
		xo = Points->x[seg];
		yo = Points->y[seg];
		
	        display_line ( line, SYMB_HIGHLIGHT, 1);
		symb_set_driver_color ( SYMB_HIGHLIGHT );
		display_icon ( xo, yo, G_ICON_BOX, 0, 10, 1);

		i_prompt_buttons ( "Confirm and select next", "Unselect", "Quit tool"); 
		last_line = line;
		last_seg = seg;
	    }
	}
	if ( button == 2 ) { /* Unselect */
	    if ( last_line > 0 ) {
		symb_set_driver_color ( SYMB_BACKGROUND );
		display_icon ( xo, yo, G_ICON_BOX, 0, 10, 1);
		last_line = 0;
	    }
	}
    }
    if ( last_line > 0 ) {
	symb_set_driver_color ( SYMB_BACKGROUND );
	display_icon ( xo, yo, G_ICON_BOX, 0, 10, 1);
    }

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL); 
    
    G_debug (3, "remove_vertex(): End");

    return 1;
}

/* Add new vertex to line */
int add_vertex (void)
{
    int i, sxn, syn, sxo = 0, syo = 0, line, last_line, last_seg, node1, node2, type, seg, np;
    int button, first, do_snap = 0;
    double x, y, px, py, thresh, xo, yo, dist, len;
    struct line_pnts *Points;
    struct line_cats *Cats;
    
    G_debug (2, "add_vertex()");

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    i_prompt ( "Add vertex:"); 
    
    driver_open();
    
    /* TODO: use some better threshold */
    thresh = fabs ( D_d_to_u_col ( 10 ) - D_d_to_u_col ( 0 ) ) ; 
    G_debug (2, "thresh = %f", thresh );
    
    first = 1; 
    last_line = 0;
    last_seg = 0;
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

	if ( button == 1 ) { /* Select line segment */
	    if ( last_line == 0 ) { /* Select line */ 
                line = Vect_find_line (&Map, x, y, 0, GV_LINE|GV_BOUNDARY, thresh, 0, 0);
                G_debug (2, "line found = %d", line );
                
                /* Display new selected line if any */
                if ( line > 0 ) {
		    display_line ( line, SYMB_HIGHLIGHT, 1);

		    /* Find the nearest vertex on the line */
		    type = Vect_read_line ( &Map, Points, NULL, line );
		    seg = Vect_line_distance ( Points, x, y, 0, 0, &px, &py, NULL, NULL, NULL, NULL );

                    G_debug (3, "seg = %d", seg );

		    xo = ( Points->x[seg-1] + Points->x[seg] ) / 2;
		    yo = ( Points->y[seg-1] + Points->y[seg] ) / 2;

		    /* If close to first or last point insert before / after the line. 
		     * 'close' is here < 1/4 of segment length */
	            do_snap = 0;
		    if ( seg == 1 ) {
		        dist = Vect_points_distance ( px, py, 0, Points->x[0], Points->y[0], 0, 0);
		        len = Vect_points_distance ( Points->x[0], Points->y[0], 0, 
				                     Points->x[1], Points->y[1], 0, 0);
                    
			if ( dist < len/4 ) {
			    seg = 0;
			    xo = Points->x[0]; 
			    yo = Points->y[0]; 
			    do_snap = 1;
			}
		    }

		    if ( seg == Points->n_points - 1 ) {
		        np = Points->n_points;
		        dist = Vect_points_distance ( px, py, 0, Points->x[np-1], Points->y[np-1], 0, 0);
		        len = Vect_points_distance  ( Points->x[np-2], Points->y[np-2], 0, 
				                      Points->x[np-1], Points->y[np-1], 0, 0);
			if ( dist < len/4 ) {
			    seg ++;
			    xo = Points->x[np-1]; 
			    yo = Points->y[np-1]; 
			    do_snap = 1;
			}
		    }
                    G_debug (3, "seg 2 = %d", seg );

                    sxo = D_u_to_d_col ( xo  ) ; 
		    syo = D_u_to_d_row ( yo );

                    i_prompt_buttons ( "New vertex", "Unselect", "Quit tool"); 
		    last_line = line;
		    last_seg = seg;
	        }
	    } else { /* Line is already selected -> new vertex */
		if ( do_snap ) {
		    snap ( &x, &y );
		}
	        display_line ( last_line, SYMB_BACKGROUND, 1);
		Vect_get_line_nodes ( &Map, last_line, &node1, &node2 ); 
                display_node ( node1, SYMB_BACKGROUND, 1);
                display_node ( node2, SYMB_BACKGROUND, 1);

		type = Vect_read_line ( &Map, Points, Cats, last_line );
		np = Points->n_points;
		/* insert vertex */
		Vect_append_point (Points, 0, 0, 0);
		for ( i = np; i > last_seg; i-- ) {
		    Points->x[i] = Points->x[i-1];
		    Points->y[i] = Points->y[i-1];
		    Points->z[i] = Points->z[i-1];
		}
		
                Points->x[last_seg] = x;
                Points->y[last_seg] = y;
                Points->z[last_seg] = 0;
		
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
    
    G_debug (3, "add_vertex(): End");

    return 1;
}

/* Move vertex */
int move_vertex (void)
{
    int sxn, syn, sxo = 0, syo = 0, line, last_line, last_seg, node1, node2, type, seg;
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
    last_seg = 0;
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
		    last_seg = seg;
	        }
	    } else { /* Line is already selected */
		if ( last_seg == 0 || last_seg == Points->n_points - 1 ) {
		    snap ( &x, &y );
		}
	        display_line ( last_line, SYMB_BACKGROUND, 1);
		Vect_get_line_nodes ( &Map, last_line, &node1, &node2 ); 
                display_node ( node1, SYMB_BACKGROUND, 1);
                display_node ( node2, SYMB_BACKGROUND, 1);

		type = Vect_read_line ( &Map, Points, Cats, last_line );
                Points->x[last_seg] = Points->x[last_seg] + x - xo;
                Points->y[last_seg] = Points->y[last_seg] + y - yo;
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
    
    G_debug (3, "move_vertex(): End");

    return 1;
}
