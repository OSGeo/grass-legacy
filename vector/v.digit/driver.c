#include "gis.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "global.h"
#include "proto.h"

int open_driver (void)
{
    int top, bot, left, right;
    
    if (R_open_driver() != 0) G_fatal_error ("No graphics device selected");

    D_setup (0);
    D_get_screen_window ( &top, &bot, &left, &right); 
    G_debug (2, "top = %d bot = %d, left = %d right = %d", top, bot, left, right);

    Xscale = ( Region.east - Region.west ) / ( right - left );
    Yscale = ( Region.north - Region.south ) / ( top - bot );

    G_debug (2, "Xscale = %f Yscale = %f", Xscale, Yscale);
    
    G_setup_plot (D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
                  D_move_abs, D_cont_abs);
    
    
    return 1;
}

int close_driver (void)
{
    R_close_driver();
    return 1;
}
