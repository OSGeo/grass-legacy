/*  @(#)get_line.c	2.1  6/26/87  */
#include "convert.h"
static int cur_screen_x, cur_screen_y ;

get_first(x, y)
	double *x, *y ;
{
	char buffer[64] ;
	double thresh ;
	int button ;
	int i ;
	int found ;
	double fabs() ;
	double	 current_thresh() ;

	cur_screen_y = get_window_top() ;
	cur_screen_x = get_window_left() ;

	Write_menu_line(6, "Buttons:") ;
	Write_menu_line(7, "Left:   where am i") ;
	Write_menu_line(8, "Middle: first point") ;
	Write_menu_line(9, "Right:  quit") ;

	thresh = current_thresh() ;

	for(;;)
	{
		R_get_location_with_pointer(&cur_screen_x, &cur_screen_y, &button) ;
		screen_to_utm ( cur_screen_x, cur_screen_y, x, y) ;

		/*******
		*x = ((double)cur_screen_x - D_west) / U_to_D_xconv + U_west ;
		*y = ((double)cur_screen_y - D_south)/ U_to_D_yconv + U_south ;
		*******/

		switch(button)
		{
			case 1:
				sprintf(buffer,"EAST: %12.2lf", *x) ;
				Write_message(1, buffer) ;
				sprintf(buffer,"NORTH: %12.2lf", *y) ;
				Write_message(2, buffer) ;
				break ;
			case 2:
				return(1) ;
				break ;
			case 3:
				return(0) ;
				break ;
		}
	}
}

assign_first(x, y)
	double x, y ;
{
	cur_screen_x = (x - U_west) * U_to_D_xconv + D_west ;
	cur_screen_y = (y - U_south) * U_to_D_yconv + D_south ;
}
	
get_next(x, y)
	double *x, *y ;
{
	char buffer[64] ;
	double thresh ;
	int button ;
	int screen_x, screen_y ;
	int i ;
	int found ;
	double	current_thresh() ;

	screen_x = cur_screen_y ;
	screen_y = cur_screen_x ;

	Write_menu_line(6, "Buttons:") ;
	Write_menu_line(7, "Left:   where am i") ;
	Write_menu_line(8, "Middle: next point") ;
	Write_menu_line(9, "Right:  quit") ;

	thresh = current_thresh() ;
	screen_x = cur_screen_x ;
	screen_y = cur_screen_y ;

	for(;;)
	{
		R_get_location_with_line(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
		screen_to_utm ( screen_x, screen_y, x, y) ;

		/******
		*x = ((double)screen_x - D_west) / U_to_D_xconv + U_west ;
		*y = ((double)screen_y - D_south)/ U_to_D_yconv + U_south ;
		******/

		switch(button)
		{
			case 1:
				sprintf(buffer,"EAST: %12.2lf", *x) ;
				Write_message(1, buffer) ;
				sprintf(buffer,"NORTH: %12.2lf", *y) ;
				Write_message(2, buffer) ;
				break ;
			case 2:
				cur_screen_y = screen_y ;
				cur_screen_x = screen_x ;
				return(1) ;
				break ;
			case 3:
				return(0) ;
				break ;
		}
	}
}
