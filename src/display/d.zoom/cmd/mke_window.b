#include "gis.h"

make_window(window)
    struct Cell_head *window ;
{
    char buffer[64] ;
    double x, y ;
    int screen_x, screen_y ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    double north,south,east,west;
    int t;
    int button ;
    int cur_screen_x, cur_screen_y ;
    double D_d_to_u_col()  ;
    double D_d_to_u_row()  ;
    double D_get_u_west() ;
    double D_get_u_south() ;
    double D_get_d_west() ;
    double D_get_d_south() ;

    screen_y = get_map_top() ;
    screen_x = get_map_left() ;

    fprintf(stderr, "\n\n");
    fprintf(stderr, "Buttons:\n") ;
    fprintf(stderr, "Left:   Establish a corner\n") ;
    fprintf(stderr, "Middle: Check coordinates\n") ;
    fprintf(stderr, "Right:  Accept region\n\n") ;

    cur_screen_x = (int)D_get_d_west() ;
    cur_screen_y = (int)D_get_d_south() ;
    screen_x = cur_screen_x + 10 ;
    screen_y = cur_screen_y + 10 ;
    ux1 = D_get_u_west() ;
    uy1 = D_get_u_south() ;

    do
    {
	R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	button &= 0xf;
	switch(button)
	{
	case 1:
		cur_screen_x = screen_x ;
		cur_screen_y = screen_y ;
		ux2 = ux1 = D_d_to_u_col((double)screen_x)  ;
		uy2 = uy1 = D_d_to_u_row((double)screen_y)  ;
		break ;
	case 2:
		uy2 = D_d_to_u_row((double)screen_y)  ;
		t = (window->north - uy2) / window->ns_res;
		uy2 = window->north - (t) * window->ns_res;
		strcpy (buffer, "?");
		G_format_northing(uy2, buffer, window->proj)  ;
		fprintf(stderr,"north: %-11s  ", buffer);

		ux2 = D_d_to_u_col((double)screen_x)  ;
		t = (ux2 - window->west) / window->ew_res;
		ux2 = window->west + (t) * window->ew_res;
		strcpy (buffer, "?");
		G_format_easting(ux2, buffer, window->proj)  ;
		fprintf(stderr,"east: %-11s\r", buffer);

		break ;
	case 3:
		ux2 = D_d_to_u_col((double)screen_x)  ;
		uy2 = D_d_to_u_row((double)screen_y)  ;
		break ;
	}
    } while (button != 3) ;

    north = uy1>uy2?uy1:uy2 ;
    south = uy1<uy2?uy1:uy2 ;
    west  = ux1<ux2?ux1:ux2 ;
    east  = ux1>ux2?ux1:ux2 ;

    t = (window->north - north) / window->ns_res;
    north = window->north - (t) * window->ns_res;

    t = (window->north - south) / window->ns_res;
    south = window->north - (t+1) * window->ns_res;

    t = (east - window->west) / window->ew_res;
    east = window->west + (t+1) * window->ew_res;

    t = (west - window->west) / window->ew_res;
    west = window->west + (t) * window->ew_res;

    window->north = north;
    window->south = south;
    window->east  = east ;
    window->west  = west ;
}
