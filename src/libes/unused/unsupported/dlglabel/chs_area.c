/*  @(#)choose_area.c	1.1  5/4/87  */
#include "dlg.h"

choose_area()
{
	char buffer[64] ;
	int screen_x, screen_y ;
	double ux, uy ;
	int button ;
	int i ;
	int choice ;
	double fabs() ;

	Write_menu_line(4, "Identify AREA to be edited") ;
	Write_menu_line(6, "Buttons:") ;
	Write_menu_line(7, "Left:   where am i") ;
	Write_menu_line(8, "Middle: choose this") ;
	Write_menu_line(9, "Right:  return") ;

	choice = 0 ;

	for(;;)
	{
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
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
				/* Check to see if we are pointing to an area */
				Write_message(3, "Checking areas") ;
				if ( choice = point_to_area(ux, uy) )
				{
					sprintf(buffer,"Chosen AREA: %d", choice) ;
					Write_message(3, buffer) ;
				}
				else
				{
					Write_message(3, "No Area found") ;
				}
				break ;
			case 3:
				return(choice) ;
				break ;
		}
	}
}
