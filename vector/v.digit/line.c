#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "global.h"
#include "proto.h"

static struct line_pnts *Points;
static struct line_cats *Cats;
static int first = 1;

/* Digitize new line */
int new_line (void)
{
    int sxo, syo, sxn, syn;
    int button;
    double x, y;
    
    G_debug (2, "new_line()");

    if ( first ) {
        Points = Vect_new_line_struct ();
        Cats = Vect_new_cats_struct ();
	first = 0;
    }
    
    i_prompt ( "Digitize new line:"); 
    i_prompt_buttons ( "New point", "New point", "Quit"); 
    Vect_reset_line ( Points );
    
    open_driver();
    R_set_update_function ( update );

    first = 1;
    while ( 1 ) {
	/* Get next coordinate */
        R_set_update_function ( update );
	if ( first ) R_get_location_with_pointer ( &sxn, &syn, &button); 
	else R_get_location_with_line (sxo, syo, &sxn, &syn, &button); 
	
	x =  D_d_to_u_col ( sxn );
	y =  D_d_to_u_row ( syn );
	G_debug (3, "button = %d x = %d = %f y = %d = %f", button, sxn, x, syn, y);
	if ( button == 0 || button == 3 ) break;

	/* Add to line */
        Vect_append_point ( Points, x, y, 0 );	

	display_points ( Points, WHITE );
	
	sxo = sxn; syo = syn;
	first = 0;
    }

    /* Write line */
    if ( button == 3 )	{
	Vect_write_line ( &Map, GV_LINE, Points, Cats );
    } else {
	display_points ( Points, BLACK );
    }

    close_driver();
    
    i_prompt (""); 
    i_prompt_buttons ( "", "", ""); 
    
    G_debug (3, "new_line(): End");

    return 1;
}

