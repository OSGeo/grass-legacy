/*  @(#)choose_line.c	1.1  5/4/87  */
#include "dlg.h"
choose_line()
{
	char buffer[64] ;
	int screen_x, screen_y ;
	double ux, uy ;
	int button ;
	int i ;

	Write_menu_line(4, "Choose LINE") ;
	Write_menu_line(6, "Buttons:") ;
	Write_menu_line(7, "Left:   where am i") ;
	Write_menu_line(8, "Middle: choose this") ;
	Write_menu_line(9, "Right:  return") ;

	for(;;)
	{
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		screen_to_utm (screen_x, screen_y, &ux, &uy) ;
		switch(button)
		{
			case 1:
				sprintf(buffer,"EAST: %12.2lf", ux) ;
				Write_message(1, buffer) ;
				sprintf(buffer,"NORTH: %12.2lf", uy) ;
				Write_message(2, buffer) ;
				break ;
			case 2:
				/* Check to see if we are pointing to a node */
				return (point_to_line(ux, uy)) ;
				break ;
			case 3:
				return(0) ;
				break ;
		}
	}
}
