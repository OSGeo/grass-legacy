/*
 * get user option
 *
 */

#define GLOBAL
#include "where.h"

do_what()
{
    struct Cell_head window ;
    char temp[128] ;
    int t, b, l, r ;
    int i, flag;

    R_open_driver();

    if (D_get_cur_wind(temp))
	G_fatal_error("No current graphics window") ;

    if (D_set_cur_wind(temp))
	G_fatal_error("Current graphics window not available") ;

/* Read in the map window associated with window */
    G_get_window(&window) ;

    if (D_check_map_window(&window))
	G_fatal_error("Setting graphics window") ;

    if (G_set_window(&window) == -1) 
	G_fatal_error("Can't set current graphics window") ;

/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting graphics window coordinates") ;
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions") ;
    
    flag = get_button();

    R_close_driver();
    return (flag);

}

get_button ()
{
    int width;
    int i;
    int row, col;
    int nrows, ncols;
    CELL *buf;
    struct Cell_head window;
    int screen_x, screen_y ;
    int button ;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;

    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;
    buf = G_allocate_cell_buf();

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

    while (1)
    {
	show_menu();
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
        return(button);
    }
}
