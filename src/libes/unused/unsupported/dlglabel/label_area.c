/*  @(#)label_area.c	2.1  6/26/87  */
#include "dlg.h"

label_area()
{
	int screen_x, screen_y ;
	int cur_screen_x, cur_screen_y ;
	double ux, uy ;
	int cat_num ;
	char buff[80] ;
	int button ;
	int choice ;
	int nogo ;
	
	Clear_menu() ;
	Clear_message() ;

	for(;;)
	{
		Clear_message() ;
		Write_message(1, "Enter category number ") ;
		Write_message(2, "  (or 0 to quit): ") ;
		Get_curses_text(buff) ;
		sscanf(buff,"%d",&cat_num) ;
		if (cat_num == 0)
		{
			Clear_message() ;
			return(0) ;
		}

		Write_menu_line(4,"AREA labeling:") ;
		Write_menu_line(6,"Use cursor to identify AREA") ;
		Write_menu_line(8, "Buttons:") ;
		Write_menu_line(9, "Left:   where am i") ;
		Write_menu_line(10, "Middle: label this area") ;
		Write_menu_line(11, "Right:  return") ;

		nogo = 0 ;
		for(;;)
		{
			R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
			screen_to_utm (screen_x, screen_y, &ux, &uy) ;
			switch(button)
			{
				case 1:
					sprintf(buff,"EAST: %12.2lf", ux) ;
					Write_message(1, buff) ;
					sprintf(buff,"NORTH: %12.2lf", uy) ;
					Write_message(2, buff) ;
					break ;
				case 2:
					Write_message(3, "Checking areas") ;
					if ( choice = point_to_area(ux, uy) )
					{
						/* erase existing label */
						/* First min. att. code */
						R_standard_color( D_translate_color("black") ) ;
						if (area[choice].n_atts)
						{
							/* First min. att. code */
							sprintf(buff, "%d", area[choice].atts[1]) ;
							Adot(&area[choice].x, &area[choice].y, buff) ;
						}
						else
						{
							area[choice].atts = (int *)(calloc(2,sizeof(int))) ;
							if (area[choice].atts == NULL)
							{
								fprintf(stderr,"ERROR: insufficient memory\n") ;
								return(-1) ;
							}
						}
						Blot(area[choice].x, area[choice].y) ;

					/* Set the new coordinates and cat num */
						area[choice].x = ux ;
						area[choice].y = uy ;
						area[choice].atts[0] = 999 ;
						area[choice].atts[1] = cat_num ;
						area[choice].n_atts = 1 ;
						R_standard_color( D_translate_color("orange") ) ;
						sprintf(buff, "%d", area[choice].atts[1]) ;
						Adot(&area[choice].x, &area[choice].y, buff) ;
						Write_area(choice, &area[choice]) ;
					}
					Write_message(3, " ") ;
					break ;
				case 3:
					nogo = 1 ;
					break ;
				default:
					break ;
			}
			if (nogo)
				break ;
		}
	}
}

unlabel_areas()
{
	int screen_x, screen_y ;
	int cur_screen_x, cur_screen_y ;
	double ux, uy ;
	char buff[80] ;
	int  button ;
	int  choice ;
	int  n ;
	
	Clear_menu() ;
	Clear_message() ;

	Write_menu_line(6,"Use cursor to identify AREA") ;
	Write_menu_line(8, "Buttons:") ;
	Write_menu_line(9, "Left:   where am i") ;
	Write_menu_line(10, "Middle: UN-label this area") ;
	Write_menu_line(11, "Right:  return") ;

	for(;;)
	{
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		screen_to_utm (screen_x, screen_y, &ux, &uy) ;
		switch(button)
		{
			case 1:
				sprintf(buff,"EAST: %12.2lf", ux) ;
				Write_message(1, buff) ;
				sprintf(buff,"NORTH: %12.2lf", uy) ;
				Write_message(2, buff) ;
				break ;

			case 2:
				Write_message(3, "Checking areas") ;
				if ( choice = point_to_area(ux, uy) )
				{
					/* erase existing label */
					/* First min. att. code */
					R_standard_color( D_translate_color("black") ) ;
					if ( ! area[choice].n_atts)
					{
						sprintf(buff, " Area (%d) not labeled", choice) ;
						Write_message(5,buff) ;
						continue ;
					}
						/* First min. att. code */
						sprintf(buff, "%d", area[choice].atts[1]) ;
						Adot(&area[choice].x, &area[choice].y, buff) ;
						Blot(area[choice].x, area[choice].y) ;

						/* Set the new coordinates and cat num */
						area[choice].x = ux ;
						area[choice].y = uy ;
						area[choice].n_atts = 0 ;
						Write_area(choice, &area[choice]) ;
				}
				Write_message(3, " ") ;
				break ;

			case 3:
				return(1) ;
				break ;

			default:
				break ;
		}
	}
}
