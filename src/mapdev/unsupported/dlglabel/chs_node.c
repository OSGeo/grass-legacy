/*  @(#)choose_node.c	1.1  5/4/87  */

#include "dlg.h"

choose_node()
{
	char buffer[64] ;
	double thresh ;
	int screen_x, screen_y ;
	double ux, uy ;
	int button ;
	int i ;
	int found ;
	int choice ;
	double fabs() ;
	double current_thresh() ;

	Clear_message() ;
	Write_message(1, "Enter NODE #,") ;
	Write_message(2, "or Enter new for new node,") ;
	Write_message(3, "or hit return to use mouse.") ;
	Write_message(4, " > ") ;
	Get_curses_text(buffer) ;
	Clear_message() ;
	if (*buffer == 'n')
		choice = tot_nodes++ ;
	else
	{
		choice = atoi(buffer) ;
		if (choice >= tot_nodes)
			choice = tot_nodes++ ;
	}

	if (choice)
	{
		if (choice >= tot_nodes)
		{
			Clear_message() ;
			Write_message(1, "You've hit the node limit.") ;
			Write_message(2, "No node chosen.") ;
			sleep(3) ;
			choice = 0 ;
		}
		if (node[choice].x == 0.0)
		{
			Clear_message() ;
			Write_message(1, "That node isn't defined.") ;
			sleep(3) ;
			choice = 0 ;
		}
		return(choice) ;
	}


	Clear_menu() ;
	Write_menu_line(6, "Buttons:") ;
	Write_menu_line(7, "Left:   where am i") ;
	Write_menu_line(8, "Middle: choose this") ;
	Write_menu_line(9, "Right:  return") ;

	thresh = current_thresh() ;

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
				/* Check to see if we are pointing to a node */
				Write_message(3, "Checking nodes") ;
				found = 0 ;
				for (i=1; i<=tot_nodes; i++)
				{
					if ( (fabs(node[i].x-ux) < thresh)
					  && (fabs(node[i].y-uy) < thresh) )
					{
						found++ ;
						choice = i ;
						sprintf(buffer,"NODE: %d", i) ;
						Write_message(3, buffer) ;
					}
				}
				if (! found)
					Write_message(3, "No Nodes found") ;
				break ;
			case 3:
				return(choice) ;
				break ;
		}
	}
}
