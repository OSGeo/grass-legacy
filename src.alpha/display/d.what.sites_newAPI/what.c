#include "what.h"

Site *closest_site();

what (once, terse)
    int once;
    int terse;
{
    int i;
    int row, col;
    int nrows, ncols;
    struct Cell_head window;
    int screen_x, screen_y ;
    double east, north, site_e;
    int button ;
    Site *close;
    char *desc;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;


    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

    do
    {
	show_buttons(once);
        R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	if (!once)
	{
	    if (button == 2) continue;
	    if (button == 3) break;
	}
        east  = D_d_to_u_col((double)screen_x) ;
        north = D_d_to_u_row((double)screen_y) ;
        row = (window.north - north) / window.ns_res ;
        col = (east - window.west) / window.ew_res ;
        if (row < 0 || row >= nrows) continue;
        if (col < 0 || col >= ncols) continue;

	if(NULL != (close = closest_site(east, north))){
	    desc =  G_site_format (close, NULL, 0);
	    fprintf(stdout, "%s\n", desc);
	}
    } while (! once) ;
}
