/*
 * get_location
 *
 */

#define GLOBAL
#include "where.h"

get_location(option,east,north)
int option;
char *east, *north;
{
    struct Cell_head window ;
    char temp[128] ;
    int t, b, l, r ;
    int i;
    int flag;

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

    if (option == 1) flag = get_button(1);
    else if (option == 2)
            {
            show_loc();
            flag = where(east,north);
            }
    else if (option == 3)
            {
            show_mouse();
            flag = where(east,north);
            }
    else flag = in_window(east,north);

    R_close_driver();
 
    return(flag);
}
