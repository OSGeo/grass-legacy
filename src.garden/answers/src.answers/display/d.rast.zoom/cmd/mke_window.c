#include "gis.h"

struct Cell_head make_window(window,name, mapset)
	struct Cell_head window ;
	char name[128], *mapset;
{
	char buffer[64] ;
	struct Cell_head window1, cur_from_db() ;
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

	G_copy (&window1, &window, sizeof window1);

	R_open_driver();

	screen_y  = get_map_top() ;
	screen_x = get_map_left() ;

	G_clear_screen() ;
	fprintf(stderr, "Buttons:\n") ;
	fprintf(stderr, "Left:   Establish a corner\n") ;
	fprintf(stderr, "Middle: Un Zoom \n") ;
	fprintf(stderr, "Right:  Accept window\n") ;

	cur_screen_x = (int)D_get_d_west() ;
	cur_screen_y = (int)D_get_d_south() ;
	screen_x = cur_screen_x + 10 ;
	screen_y = cur_screen_y + 10 ;
	ux1 = (int)D_get_u_west() ;
	uy1 = (int)D_get_u_south() ;

	do
	{
		R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
		switch(button & 0x0f)
		{
		case 1:
			cur_screen_x = screen_x ;
			cur_screen_y = screen_y ;
			ux2 = ux1 = (int)D_d_to_u_col((double)screen_x)  ;
			uy2 = uy1 = (int)D_d_to_u_row((double)screen_y)  ;
			break ;
		case 2:
			R_standard_color(D_translate_color("black"));
			D_erase_window();
			D_clear_window();
			R_close_driver();

			window = cur_from_db();
			/*
			R_open_driver();

			D_clear_window();
			window = cur_from_db();
			G_put_window(&window);
			G_set_window(&window);
			G_get_set_window(&window);
			D_check_map_window(&window);
			Dcell(name,mapset,0);
			R_close_driver();
			*/
			unzoom(window,name,mapset);
			exit(0);
		case 3:
			ux2 = (int)D_d_to_u_col((double)screen_x)  ;
			uy2 = (int)D_d_to_u_row((double)screen_y)  ;
			break ;
		}
	} while (button != 3) ;

	north = uy1>uy2?uy1:uy2 ;
	south = uy1<uy2?uy1:uy2 ;
	west  = ux1<ux2?ux1:ux2 ;
	east  = ux1>ux2?ux1:ux2 ;

	t = (window.north - north) / window.ns_res;
	north = window.north - (t) * window.ns_res;

	t = (window.north - south) / window.ns_res;
	south = window.north - (t+1) * window.ns_res;

	t = (east - window.west) / window.ew_res;
	east = window.west + (t+1) * window.ew_res;

	t = (west - window.west) / window.ew_res;
	west = window.west + (t) * window.ew_res;

	window.north = north;
	window.south = south;
	window.east  = east ;
	window.west  = west ;
	cur_to_db(&window1);
	return(window);
}
