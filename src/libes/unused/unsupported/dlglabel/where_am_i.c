/*  @(#)where_am_i.c	2.1  6/26/87  */
#include "dlg.h"

where_am_i()
{
	int i ;
	int found ;
	int button ;
	int screen_x, screen_y ;
	double thresh ;
	double ux, uy ;
	char buffer[64] ;

	double fabs() ;
	double current_thresh() ;

	screen_y  = 0 ;
	screen_x = 0 ;

	Write_menu_line(6, "Buttons:") ;
	Write_menu_line(7, "Left:   where am i") ;
	Write_menu_line(8, "Middle: identify this") ;
	Write_menu_line(9, "Right:  quit") ;

	thresh =  current_thresh() ;

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
				/* Check to see if we are pointing to an area */
				Write_message(3, "Checking areas") ;
				if ( i = point_to_area(ux, uy) )
				{
					sprintf(buffer,"AREA: %d", i) ;
					Write_area(i, &area[i]) ;
					Write_message(3, buffer) ;
				}
				else
					Write_message(3, "No Area found") ;

				/* Check to see if we are pointing to a node */
				Write_message(4, "Checking nodes") ;
				found = 0 ;
				for (i=1; i<=tot_nodes; i++)
				{
					if ( (fabs(node[i].x-ux) < thresh)
					  && (fabs(node[i].y-uy) < thresh) )
					{
						found++ ;
						sprintf(buffer,"NODE: %d", i) ;
						Write_node(i, &node[i]) ;
						Write_message(4, buffer) ;
					}
				}
				if (! found)
					Write_message(4, "No Nodes found") ;

				/* Check to see if we are pointing to a line */
				/*  checking for lines takes a long time  
				* 	may have to comment out or find a faster way.
				*/
				Write_message(5, "Checking lines") ;
				if ( i = point_to_line(ux, uy) )
				 {
					sprintf(buffer,"LINE: %d", i) ;
					Write_line(i, &line[i]) ;
					Write_message(5, buffer) ;
				 }
				else
					Write_message(5, "No Lines found") ;
				break ;

			case 3:
				return(0) ;
				break ;
		}
	}
}
