#include "gis.h"
#include "raster.h"
#include "display.h"
#include "D.h" 
#include "colors.h"
#include "global.h"
#include "proto.h"


int driver_refresh (void)
{
    D_setup (0);
    G_setup_plot (D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
		                  D_move_abs, D_cont_abs);
    return 1;
}
    
int driver_open (void)
{
    int top, bot, left, right;
    double n, s, e, w;
    
    G_debug (5, "driver_open()");
    if (R_open_driver() != 0) G_fatal_error ("No graphics device selected");
    G_debug (5, " -> opened");

    D_setup (0);
    D_get_screen_window ( &top, &bot, &left, &right); 
    G_debug (2, "top = %d bot = %d, left = %d right = %d", top, bot, left, right);

    
    G_debug (2, "n = %f s = %f, w = %f e = %f", D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east() );
    n = D_d_to_u_row ( D_get_d_north() ); 
    s = D_d_to_u_row ( D_get_d_south() ); 
    w = D_d_to_u_col ( D_get_d_west() );
    e = D_d_to_u_col (  D_get_d_east() );
    G_debug (2, "n = %f s = %f, w = %f e = %f", n, s, w, e );
    
    Scale = (n - s) / ( D_get_d_south() - D_get_d_north() );
    G_debug (2, "Scale = %f", Scale);
    /*
    Xscale = ( Region.east - Region.west ) / ( right - left );
    Yscale = ( Region.north - Region.south ) / ( top - bot );

    G_debug (2, "Xscale = %f Yscale = %f", Xscale, Yscale);
    */
    G_setup_plot (D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
                  D_move_abs, D_cont_abs);
    
    D_set_clip_window_to_map_window ();
    
    return 1;
}

int driver_close (void)
{
    G_debug (5, "driver_close()");
    R_close_driver();
    G_debug (5, " -> closed");
    return 1;
}

