#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "global.h"
#include "proto.h"

/* Zoom - select new window interactively in the monitor */
int zoom_window (void)
{
    int sxo, syo, sxn, syn;
    int button;
    int mode, next_mode; /* 1 - first corner; 2 - first or second corner */
    double x1, y1, x2, y2;
    
    G_debug (2, "zoom()");

    i_prompt ( "Zoom by window"); 
    i_prompt_buttons ( "1. corner", "1. corner", "Quit"); 
    
    driver_open();

    mode = 1;
    sxn = COOR_NULL; syn = COOR_NULL; 
    while ( 1 ) {
        R_set_update_function ( update );
        if ( mode == 1 ) {
	    R_get_location_with_pointer ( &sxn, &syn, &button); 
            i_prompt_buttons ( "1. corner", "2. corner", "Quit"); 
	    next_mode = 2;
	} else { 
	    R_get_location_with_box(sxo, syo, &sxn, &syn, &button) ; 
        }

	
	G_debug (2, "button = %d x = %d y = %d", button, sxn, syn);

	if ( button == 0 || button == 3 ) break;
	
	if ( mode == 2 && button == 2 ) {
	    x1 =  D_d_to_u_col ( sxo );
	    y1 =  D_d_to_u_row ( syo );
	    x2 =  D_d_to_u_col ( sxn );
	    y2 =  D_d_to_u_row ( syn );
	
	    G_debug (2, "x1 = %f x2 = %f y1 = %f y2 = %f", x1, x2, y1, y2);

	    window.north = y1 > y2 ? y1 : y2 ;
	    window.south = y1 < y2 ? y1 : y2 ;
	    window.west  = x1 < x2 ? x1 : x2 ;
	    window.east  = x1 > x2 ? x1 : x2 ;
    
	    G_debug (2, "w = %f e = %f n = %f s = %f", window.west, window.east, window.north, window.south);

	    G_adjust_Cell_head (&window, 0, 0);
	    G_put_window(&window);
	    G_set_window(&window);

	    display_redraw();

	    i_prompt_buttons ( "1. corner", "1. corner", "Quit"); 
	    next_mode = 1;
	}

	sxo = sxn; syo = syn;
	mode = next_mode;
	
    }

    driver_close();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    i_coor ( COOR_NULL, COOR_NULL);
    
    G_debug (3, "zoom_window(): End");

    return 1;
}

/* Zoom - in / out (centre unchanged) */
int zoom_centre ( double factor)
{
    double xc, yc, dx, dy;
    
    G_debug (2, "zoom_centre()");

    driver_open();
    R_set_update_function ( update );

    G_debug (2, "1 n = %f s = %f", window.north, window.south);

    dx = (window.east - window.west) / 2;
    dy = (window.north - window.south) / 2;
    xc = (window.east + window.west) / 2;
    yc = (window.north + window.south) / 2;
	
    G_debug (2, "  yc = %f dy = %f", yc, dy);
    
    window.north = yc + dy * factor;
    window.south = yc - dy * factor;
    window.east  = xc + dx * factor;
    window.west  = xc - dx * factor;
    
    
    G_debug (2, "2 n = %f s = %f", window.north, window.south);
    G_adjust_Cell_head (&window, 0, 0);
    G_debug (2, "3 n = %f s = %f", window.north, window.south);
    G_put_window(&window);
    G_set_window(&window);
    
    display_redraw();

    driver_close();
    
    G_debug (3, "zoom_centre(): End");

    return 1;
}

