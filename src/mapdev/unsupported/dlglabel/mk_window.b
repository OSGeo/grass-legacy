/*  @(#)make_window.c	1.1  5/4/87  */
#include "convert.h"
make_window()
{
	char buffer[64] ;
	double x, y ;
	int screen_x, screen_y ;
	double ux1, uy1 ;
	double ux2, uy2 ;
	int button ;
	int cur_screen_x, cur_screen_y ;

	Write_menu_line(5, "Buttons:") ;
	Write_menu_line(6, "Left:   Establish a corner") ;
	Write_menu_line(7, "Middle: Check coordinates") ;
	Write_menu_line(8, "Right:  Accept window") ;

	cur_screen_x = (int)D_west ;
	cur_screen_y = (int)D_south ;
	screen_x = cur_screen_x + 10 ;
	screen_y = cur_screen_y + 10 ;
	ux1 = U_west ;
	uy1 = U_south ;

	do
	{
		R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
		if(button == 1)
		{
			cur_screen_x = screen_x ;
			cur_screen_y = screen_y ;
			screen_to_utm ( cur_screen_x, cur_screen_y, &ux1, &uy1) ;
		}

		screen_to_utm ( screen_x, screen_y, &ux2, &uy2) ;

		sprintf(buffer,"NORTH: %10.2f", uy1>uy2?uy1:uy2) ;
			Write_menu_line(10, buffer) ;
		sprintf(buffer,"SOUTH: %10.2f", uy1<uy2?uy1:uy2) ;
			Write_menu_line(11, buffer) ;
		sprintf(buffer,"WEST:  %10.2f", ux1<ux2?ux1:ux2) ;
			Write_menu_line(12, buffer) ;
		sprintf(buffer,"EAST:  %10.2f", ux1>ux2?ux1:ux2) ;
			Write_menu_line(13, buffer) ;

	} while (button != 3) ;

/* This redefines the current window */
	Write_message(1, "Save coordinates as your") ;
	Write_message(2, "Current Window?") ;
	Write_message(3, "y/n> ") ;

	Get_curses_text(buffer) ;
	switch (buffer[0])
	{
		case 'y':
			U_west  =  ux1<ux2?ux1:ux2 ;
			U_east  =  ux1>ux2?ux1:ux2 ;
			U_south =  uy1<uy2?uy1:uy2 ;
			U_north =  uy1>uy2?uy1:uy2 ;
			return(0) ;
			break ;
		default:
			return(1) ;
			break ;
	}
}
