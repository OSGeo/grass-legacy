/*  @(#)get_n_coor.c	2.1  6/26/87  */
#include "dlg.h"
#include "convert.h"

get_new_coor(oldx, oldy, x, y)
	double oldx, oldy ;
	double *x, *y ;
{
	int screen_x, screen_y ;
	int cur_screen_x, cur_screen_y ;
	double ux, uy ;
	double atof() ;
	char buffer[128] ;
	int button ;

	ux = 0.0 ;
	while( (ux < U_west) || (ux > U_east) )
	{
		Write_message(1, "Enter new x coordinate,") ;
		Write_message(2, "or hit return to use mouse.") ;
		Write_message(3, " > ") ;
		Get_curses_text(buffer) ;
		Clear_message() ;
		ux = atof(buffer) ;
		if (ux == 0.0)
			break ;
	}
	if (ux != 0.0)
		while( (uy < U_south) || (uy > U_north) )
		{
			Write_message(1, "Enter new y coordinate,") ;
			Write_message(3, " > ") ;
			Get_curses_text(buffer) ;
			Clear_message() ;
			uy = atof(buffer) ;
		}
	if (ux != 0.0)
	{
		*x = ux ;
		*y = uy ;
		return(1) ;
	}


	Clear_menu() ;
	Write_menu_line(6, "Buttons:") ;
	Write_menu_line(7, "Left:   where am i") ;
	Write_menu_line(8, "Middle: choose this") ;
	Write_menu_line(9, "Right:  quit") ;

	screen_x = cur_screen_x = (oldx - U_west)  * U_to_D_xconv + D_west ;
	screen_y = cur_screen_y = (oldy - U_south) * U_to_D_yconv + D_south ;

	for(;;)
	{
		R_get_location_with_line(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
		screen_to_utm ( screen_x, screen_y, &ux, &uy) ;

		switch(button)
		{
			case 1:
				sprintf(buffer,"EAST: %12.2lf", ux) ;
				Write_message(1, buffer) ;
				sprintf(buffer,"NORTH: %12.2lf", uy) ;
				Write_message(2, buffer) ;
				break ;
			case 2:
				*x = ux ;
				*y = uy ;
				Clear_message() ;
				return(1) ;
			case 3:
				Clear_message() ;
				return(0) ;
				break ;
		}
	}
}
